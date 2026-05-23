{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure datum helpers in 'TxBuilding.Toa.Conversions' and the
-- JSON wire shape of 'Api.Types.UtxoSummary' that composes them.
--
-- The wire contract is locked down here: 'outDatumCborBytes' returns the
-- canonical CBOR of an inline datum (3 bytes @d8 79 80@ for the unit datum),
-- and 'outDatumHashBytes' returns blake2b_256 of those bytes. The well-known
-- unit-datum hash is hard-coded; if Atlas's 'hashDatum' or 'datumToApi'' ever
-- changes its encoding, this test will fail loudly. The 'UtxoSummary' group
-- additionally pins the @datum_cbor_hex@/@datum_hash@ JSON field names and
-- asserts the legacy @has_inline_datum@ boolean is gone.
module UnitTests.Conversions
  ( conversionsTests
  ) where

import Api.AppMonad (toServerError)
import Api.Types (UtxoSummary (..))
import Data.Aeson (encode, object, (.=))
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import GeniusYield.Types
  ( GYOutDatum (..)
  , GYTxOutRef
  , GYValue
  , datumHashFromHexE
  , txOutRefFromTuple
  , unitDatum
  , valueFromLovelace
  )
import Onchain.Protocol.Types (TOAParamsV1 (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Servant (ServerError (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Toa.Conversions (outDatumCborBytes, outDatumHashBytes, paramsCborBytes)
import Utils (hexBytes, hexText)

-- | CBOR of the unit datum @Constr 0 []@ — three bytes: tag 121 + empty array.
unitDatumCborHex :: ByteString
unitDatumCborHex = "d87980"

-- | blake2b_256 of @d87980@. Verified out-of-band:
-- @python3 -c "import hashlib; print(hashlib.blake2b(bytes.fromhex('d87980'), digest_size=32).hexdigest())"@
unitDatumHashHex :: ByteString
unitDatumHashHex = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"

conversionsTests :: TestTree
conversionsTests =
  testGroup
    "TxBuilding.Toa.Conversions"
    [ canonicalCborTests
    , errorMappingTests
    , testGroup "outDatumCborBytes"
        [ testCase "inline unit datum → 3-byte CBOR" $
            assertEqual
              "expected canonical unit-datum CBOR"
              (Just unitDatumCborHex)
              (fmap hexBytes (outDatumCborBytes (GYOutDatumInline unitDatum)))
        , testCase "hash-only output → Nothing" $ do
            h <- either (fail . show) pure (datumHashFromHexE (replicate 64 '0'))
            assertEqual
              "hash-only outputs have no inline CBOR"
              Nothing
              (outDatumCborBytes (GYOutDatumHash h))
        , testCase "no datum → Nothing" $
            assertEqual
              "absent datum has no CBOR"
              Nothing
              (outDatumCborBytes GYOutDatumNone)
        ]
    , testGroup "outDatumHashBytes"
        [ testCase "inline unit datum → known blake2b_256" $
            assertEqual
              "expected unit-datum hash"
              (Just unitDatumHashHex)
              (fmap hexBytes (outDatumHashBytes (GYOutDatumInline unitDatum)))
        , testCase "hash-only output → echoes the supplied hash" $ do
            let knownHex :: String
                knownHex = "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
            h <- either (fail . show) pure (datumHashFromHexE knownHex)
            assertEqual
              "hash-only output exposes its ledger hash unchanged"
              (Just (BS8.pack knownHex))
              (fmap hexBytes (outDatumHashBytes (GYOutDatumHash h)))
        , testCase "no datum → Nothing" $
            assertEqual
              "absent datum has no hash"
              Nothing
              (outDatumHashBytes GYOutDatumNone)
        ]
    , utxoSummaryWireTests
    ]

-------------------------------------------------------------------------------
-- paramsCborBytes — RFC 8949 §4.2.1 canonical CBOR
-------------------------------------------------------------------------------

-- | Pin the canonical-CBOR encoding of 'TOAParamsV1'. The TOA CIP
-- §"Canonical Parameter Encoding" requires definite-length items and
-- smallest-form integers (RFC 8949 §4.2.1). F-L replaced the indefinite-
-- length 'Codec.Serialise.serialise' encoding with a direct cborg
-- 'Encoding'; this group locks the encoder in place against drift.
canonicalCborTests :: TestTree
canonicalCborTests =
  testGroup
    "paramsCborBytes — RFC 8949 §4.2.1 canonical CBOR"
    [ testCase "tag 121, definite-length array, smallest-form integer" $ do
        let policy = CurrencySymbol (toBuiltin (BS.replicate 28 0x11))
            asset  = TokenName (toBuiltin (BS.pack [0x54, 0x4f, 0x41]))   -- "TOA"
            bytes  = paramsCborBytes (TOAParamsV1 1 policy asset)
            -- d8 79     : tag 121
            -- 83        : definite-length array of 3
            -- 01        : uint 1 (smallest form)
            -- 58 1c     : bytes(28)
            -- 11...11   : 28 0x11 bytes
            -- 43        : bytes(3)
            -- 54 4f 41  : "TOA"
            expectedHex =
              "d8798301581c" <> Text.replicate 28 "11"
              <> "43544f41"
        assertEqual "canonical CBOR shape"
          expectedHex
          (hexText bytes)
    , testCase "starts with definite-length tag+array marker" $ do
        let bytes = paramsCborBytes (TOAParamsV1 1
                      (CurrencySymbol (toBuiltin (BS.replicate 28 0x00)))
                      (TokenName (toBuiltin BS.empty)))
        BS.take 3 bytes @?= BS.pack [0xd8, 0x79, 0x83]
    ]

-------------------------------------------------------------------------------
-- toServerError mapping (TxBuildingException -> HTTP status)
-------------------------------------------------------------------------------

-- | Pin the HTTP-status table of 'toServerError'. F-B unified two prior
-- mapping paths into a single function consulting
-- 'txBuildingExceptionToHttpStatus' for the status and routing through
-- the matching @err400@/@err404@/etc. constructor; this group locks the
-- table in place against accidental drift.
errorMappingTests :: TestTree
errorMappingTests =
  testGroup
    "toServerError (TxBuildingException -> HTTP status)"
    [ testCase "NFTNotFound -> 404" $
        errHTTPCode (toServerError NFTNotFound) @?= 404
    , testCase "InsufficientToaValue -> 422" $
        errHTTPCode
          (toServerError (InsufficientToaValue (valueFromLovelace 1) (valueFromLovelace 0)))
          @?= 422
    , testCase "MultipleUtxosFound -> 400" $
        errHTTPCode (toServerError MultipleUtxosFound) @?= 400
    , testCase "InvalidAssetClass -> 400" $
        errHTTPCode (toServerError InvalidAssetClass) @?= 400
    , testCase "InvalidParams -> 400" $
        errHTTPCode (toServerError (InvalidParams "x")) @?= 400
    , testCase "ProviderError -> 502" $
        errHTTPCode (toServerError (ProviderError "x")) @?= 502
    ]

-------------------------------------------------------------------------------
-- UtxoSummary JSON wire shape
-------------------------------------------------------------------------------

-- NOTE: These tests pin the JSON shape of 'UtxoSummary' values constructed
-- directly. The 'Api.Handlers.Toa.summarise' projection from a 'GYUTxO' is
-- intentionally not exercised here — it is a three-line composition of the
-- already-tested 'outDatumCborBytes' / 'outDatumHashBytes' helpers, and its
-- @GYUTxO@-construction would require fixture machinery out of scope for a
-- pure unit test. Cover the projection at the integration level if needed.
utxoSummaryWireTests :: TestTree
utxoSummaryWireTests =
  testGroup
    "UtxoSummary JSON wire shape"
    [ testCase "inline-datum UTxO encodes datum_cbor_hex and datum_hash" $ do
        let summary  = sampleSummary (Just unitDatumCborHexText) (Just unitDatumHashHexText)
            encoded  = A.toJSON summary
            expected =
              object
                [ "ref"            .= sampleRef
                , "value"          .= sampleValue
                , "datum_cbor_hex" .= unitDatumCborHexText
                , "datum_hash"     .= unitDatumHashHexText
                ]
        assertEqual "inline-datum wire shape" expected encoded
    , testCase "hash-only UTxO encodes null cbor + hash present" $ do
        let summary  = sampleSummary Nothing (Just unitDatumHashHexText)
            encoded  = A.toJSON summary
            expected =
              object
                [ "ref"            .= sampleRef
                , "value"          .= sampleValue
                , "datum_cbor_hex" .= (Nothing :: Maybe Text)
                , "datum_hash"     .= unitDatumHashHexText
                ]
        assertEqual "hash-only wire shape" expected encoded
    , testCase "no-datum UTxO encodes both fields as null" $ do
        let summary  = sampleSummary Nothing Nothing
            encoded  = A.toJSON summary
            expected =
              object
                [ "ref"            .= sampleRef
                , "value"          .= sampleValue
                , "datum_cbor_hex" .= (Nothing :: Maybe Text)
                , "datum_hash"     .= (Nothing :: Maybe Text)
                ]
        assertEqual "no-datum wire shape" expected encoded
    , testCase "no field named has_inline_datum is present" $ do
        let summary = sampleSummary Nothing Nothing
            bs      = encode summary
        assertEqual
          "legacy boolean field has been removed"
          False
          ("has_inline_datum" `BS.isInfixOf` BSL.toStrict bs)
    ]
  where
    -- Tx-hash is a 64-char hex literal. 'GYTxId' has an 'IsString' instance
    -- (Atlas — see GeniusYield.Types.Tx), so the literal is inferred as
    -- @GYTxId@ in the tuple context expected by 'txOutRefFromTuple'.
    sampleRef :: GYTxOutRef
    sampleRef =
      txOutRefFromTuple
        ("0000000000000000000000000000000000000000000000000000000000000000", 0)

    sampleValue :: GYValue
    sampleValue = valueFromLovelace 1500000

    sampleSummary :: Maybe Text -> Maybe Text -> UtxoSummary
    sampleSummary cbor hash =
      UtxoSummary
        { usRef          = sampleRef
        , usValue        = sampleValue
        , usDatumCborHex = cbor
        , usDatumHash    = hash
        }

    unitDatumCborHexText :: Text
    unitDatumCborHexText = TE.decodeUtf8 unitDatumCborHex

    unitDatumHashHexText :: Text
    unitDatumHashHexText = TE.decodeUtf8 unitDatumHashHex
