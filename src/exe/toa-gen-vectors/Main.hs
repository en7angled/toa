{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Emit TOA v1 normative artifacts:
--
--   * @validators/ToaV1.uplc@   — raw un-applied UPLC bytes (single CBOR bytestring).
--   * @test-vectors/toa-v1.json@ — six address-derivation vectors.
--
-- Pure in-process generation; does not contact a node. Run from repo root with
-- @cabal run toa-gen-vectors@.
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.List (maximumBy)
import Data.Maybe qualified
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Deriving.Aeson (CamelToSnake, CustomJSON (..), FieldLabelModifier, StripPrefix)
import GHC.Generics (Generic)
import GeniusYield.Types
  ( GYNetworkId (..),
    GYScriptHash,
    addressToText,
    mintingPolicyIdFromCurrencySymbol,
    scriptHashToApi,
    tokenNameFromPlutus,
  )
import Onchain.Protocol.Types (TOAParamsV1 (..))
import Onchain.Validators.ToaV1Validator (toaV1ApplyConstantData)
import TxBuilding.Toa.DerivationR qualified as R
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import TxBuilding.Toa.Address (toaAddress)
import TxBuilding.Toa.Conversions (paramsCborBytes)
import TxBuilding.Toa.Fingerprint qualified as Lib
import Utils (currencySymbolBytes, hexText, rawBytes, tokenNameBytes)
import TxBuilding.Toa.Validator
  ( toaV1ScriptHash,
    toaV1UnappliedBytes,
    toaV1UnappliedHashBytes,
    writeUnappliedBytes,
  )

-------------------------------------------------------------------------------
-- Test vector data
-------------------------------------------------------------------------------

-- | Fixed sample policy id (28 bytes) so vectors are byte-reproducible.
samplePolicy :: CurrencySymbol
samplePolicy =
  mkCS (decodeHex "01234567890123456789012345678901234567890123456789012345")

-- | CIP-67 label-100 4-byte prefix.
cip67Label100 :: ByteString
cip67Label100 = BS.pack [0x00, 0x06, 0x43, 0xb0]

-- | CIP-67 label-222 4-byte prefix.
cip67Label222 :: ByteString
cip67Label222 = BS.pack [0x00, 0x0d, 0xe1, 0x40]

vectors :: [(Text, TOAParamsV1)]
vectors =
  [ ("ascii", TOAParamsV1 1 samplePolicy (mkTN "TOA Test NFT 001")),
    ("empty", TOAParamsV1 1 samplePolicy (mkTN BS.empty)),
    ("max32", TOAParamsV1 1 samplePolicy (mkTN (BS.replicate 32 0xAB))),
    ("cip67-100", TOAParamsV1 1 samplePolicy (mkTN (cip67Label100 <> "RefToken"))),
    ("cip67-222", TOAParamsV1 1 samplePolicy (mkTN (cip67Label222 <> "UserToken"))),
    ("ascii-v2", TOAParamsV1 2 samplePolicy (mkTN "TOA Test NFT 001"))
  ]

-------------------------------------------------------------------------------
-- Output record
-------------------------------------------------------------------------------

-- Field names use a "v" prefix to avoid colliding with imported names; the
-- deriving-aeson modifier strips it and snake-cases the rest, so JSON keys
-- match the CIP shape ("name", "toa_version", ...).
data Vector = Vector
  { vName :: Text,
    vToaVersion :: Integer,
    vPolicyId :: Text,
    vAssetNameHex :: Text,
    vCip14Fingerprint :: Text,
    vParamsCborHex :: Text,
    vUnappliedScriptHash :: Text,
    vAppliedScriptCborHex :: Text,
    vAppliedScriptBytes :: Int,
    vFlatBodyLength :: Int,
    vExpectedScriptHash :: Text,
    vExpectedAddressMainnet :: Text,
    vExpectedAddressTestnet :: Text,
    vDatumPolicy :: Text,
    vSelfDepositSemantics :: Text
  }
  deriving stock (Generic)
  deriving
    (Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "v", CamelToSnake]] Vector

-- | Top-level JSON envelope wrapping the vector list. Pulls the unapplied
-- UPLC size (a single global value, shared by every vector) and the reference-
-- script ceiling out of the per-vector records so consumers can read them
-- once. Cardano's reference-script limit is 16,384 bytes.
data VectorEnvelope = VectorEnvelope
  { eUnappliedScriptBytes :: Int,
    eMaxReferenceScriptBytes :: Int,
    eVectors :: [Vector]
  }
  deriving stock (Generic)
  deriving
    (Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "e", CamelToSnake]] VectorEnvelope

-- | Cardano's reference-script size ceiling per transaction.
maxReferenceScriptBytes :: Int
maxReferenceScriptBytes = 16384

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------

main :: IO ()
main = do
  createDirectoryIfMissing True "validators"
  createDirectoryIfMissing True "test-vectors"
  writeUnappliedBytes "validators/ToaV1.uplc"
  putStrLn $ "Template hash (un-applied):  " <> Text.unpack (hex toaV1UnappliedHashBytes)
  putStrLn $ "Wrote validators/ToaV1.uplc (" <> show (BS.length toaV1UnappliedBytes) <> " bytes)"
  let vs = map mkVector vectors
  mapM_ (\Vector {..} -> putStrLn $ Text.unpack vName <> "\t" <> Text.unpack vExpectedAddressMainnet) vs
  let cfg = defConfig {confIndent = Spaces 2, confTrailingNewline = True}
      envelope =
        VectorEnvelope
          { eUnappliedScriptBytes = BS.length toaV1UnappliedBytes,
            eMaxReferenceScriptBytes = maxReferenceScriptBytes,
            eVectors = vs
          }
  BSL.writeFile "test-vectors/toa-v1.json" (encodePretty' cfg envelope)
  putStrLn $ "Wrote test-vectors/toa-v1.json (" <> show (length vs) <> " vectors)"
  let unappliedSize = BS.length toaV1UnappliedBytes
      unappliedPct = fromIntegral unappliedSize / fromIntegral maxReferenceScriptBytes * 100 :: Double
      (maxName, maxSize) = maximumBy (comparing snd) [(vName v, vAppliedScriptBytes v) | v <- vs]
      appliedPct = fromIntegral maxSize / fromIntegral maxReferenceScriptBytes * 100 :: Double
  printf "Unapplied UPLC: %d bytes (%.1f%% of %d)\n" unappliedSize unappliedPct maxReferenceScriptBytes
  printf "Max applied:    %d bytes (%.1f%% of %d) — vector %s\n" maxSize appliedPct maxReferenceScriptBytes (show (Text.unpack maxName))

mkVector :: (Text, TOAParamsV1) -> Vector
mkVector (n, p@TOAParamsV1 {..}) =
  let appliedBytes = SBS.fromShort (toaV1ApplyConstantData p)
      paramBytes   = paramsCborBytes p
      -- flat_body_length = len(FLAT_PREFIX) + 1 (chunk-length byte)
      --                  + len(paramCbor) + 1 (0x00 chunk-terminator)
      --                  + len(FLAT_SUFFIX)
      flatBodyLen  = BS.length R.flatPrefixToaV1
                   + 1
                   + BS.length paramBytes
                   + 1
                   + BS.length R.flatSuffixToaV1
   in Vector
        { vName = n,
          vToaVersion = toaVersion,
          vPolicyId = hex (currencySymbolBytes toaPolicyId),
          vAssetNameHex = hex (tokenNameBytes toaAssetName),
          vCip14Fingerprint =
            Lib.cip14Fingerprint
              (either (error . show) id (mintingPolicyIdFromCurrencySymbol toaPolicyId))
              (Data.Maybe.fromMaybe (error "tokenNameFromPlutus: asset name too large") (tokenNameFromPlutus toaAssetName)),
          vParamsCborHex = hex paramBytes,
          vUnappliedScriptHash = hex toaV1UnappliedHashBytes,
          vAppliedScriptCborHex = hex appliedBytes,
          vAppliedScriptBytes = BS.length appliedBytes,
          vFlatBodyLength = flatBodyLen,
          vExpectedScriptHash = hex (scriptHashRawBytes (toaV1ScriptHash p)),
          vExpectedAddressMainnet = addressToText (toaAddress GYMainnet p),
          vExpectedAddressTestnet = addressToText (toaAddress GYTestnetPreprod p),
          vDatumPolicy = "inline_unit_recommended",
          vSelfDepositSemantics = "controlled_by_external_nft_holder"
        }

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mkCS :: ByteString -> CurrencySymbol
mkCS = CurrencySymbol . toBuiltin

mkTN :: ByteString -> TokenName
mkTN = TokenName . toBuiltin

hex :: ByteString -> Text
hex = hexText

decodeHex :: ByteString -> ByteString
decodeHex bs = case Base16.decode bs of
  Right ok -> ok
  Left e -> error ("toa-gen-vectors: bad hex literal: " <> e)

scriptHashRawBytes :: GYScriptHash -> ByteString
scriptHashRawBytes = rawBytes . scriptHashToApi
