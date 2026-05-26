{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Pure-Haskell mirror of the canonical byte-level TOA v1 address-derivation
-- function R. Computes the same 28-byte script hash that on-chain
-- 'serialiseData' + 'blake2b_224' would compute for an applied TOA v1
-- validator script — without ever running PlutusTx, the PlutusTx plugin, or
-- @apply_params@.
--
-- The reconstruction reproduces the flat-encoded UPLC layout empirically
-- verified by @toa-verify-reconstruction@:
--
-- @
--   applied = CBOR_HDR(n) || FLAT_PREFIX || chunkedFrame(paramCbor) || FLAT_SUFFIX
--   chunkedFrame(b) = consByteString(len(b), \"\") || b || 0x00
-- @
--
-- where @len(b) <= 255@ (always true for TOA v1: max paramCbor ~68 B).
--
-- See TOA CIP §\"Address Derivation\".
module TxBuilding.Toa.DerivationR
  ( toaScriptHash
  , flatPrefixToaV1
  , flatSuffixToaV1
  ) where

import           Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import           Cardano.Crypto.Hash.Class   (hashToBytes, hashWith)
import qualified Codec.Serialise             as Ser
import           Data.Bits                   (shiftR, (.&.))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.FileEmbed              (embedFile)
import           PlutusLedgerApi.V3          (Data (..))

-- | Invariant 475-byte prefix extracted from @validators/ToaV1.uplc@.
flatPrefixToaV1 :: ByteString
flatPrefixToaV1 = $(embedFile "validators/FLAT_PREFIX_TOA_V1.bin")

-- | Invariant 1-byte suffix (@0x01@).
flatSuffixToaV1 :: ByteString
flatSuffixToaV1 = $(embedFile "validators/FLAT_SUFFIX_TOA_V1.bin")

-- | RFC 8949 canonical major-type-2 (bytestring) header for length @n@.
cborByteStringHeader :: Int -> ByteString
cborByteStringHeader n
  | n <= 0x17       = BS.singleton (fromIntegral (0x40 + n))
  | n <= 0xff       = BS.pack [0x58, fromIntegral n]
  | n <= 0xffff     = BS.pack [0x59, fromIntegral (n `shiftR` 8), fromIntegral (n .&. 0xff)]
  | n <= 0xffffffff =
      BS.pack [ 0x5a
              , fromIntegral (n `shiftR` 24 .&. 0xff)
              , fromIntegral (n `shiftR` 16 .&. 0xff)
              , fromIntegral (n `shiftR`  8 .&. 0xff)
              , fromIntegral (n             .&. 0xff)
              ]
  | otherwise = error "cborByteStringHeader: length >= 2^32 not supported by TOA v1"

-- | Canonical PlutusData CBOR of @TOAParamsV1@ — byte-identical to what the
-- on-chain 'serialiseData' builtin produces. plutus-core's 'Serialise'
-- instance for 'Data' is the same implementation that backs @serialiseData@,
-- so this is the encoder identity used by R.
serialiseToaParams :: Integer -> ByteString -> ByteString -> ByteString
serialiseToaParams toaVersion policyId assetName =
  BSL.toStrict (Ser.serialise (Constr 0 [I toaVersion, B policyId, B assetName]))

-- | Canonical byte-level TOA v1 address-derivation function R.
toaScriptHash
  :: Integer    -- ^ toa_version
  -> ByteString -- ^ policy_id (28 bytes)
  -> ByteString -- ^ asset_name (0..32 bytes)
  -> ByteString -- ^ 28-byte script hash (payment credential)
toaScriptHash toaVersion policyId assetName
  | BS.length paramCbor > 255 =
      error ("toaScriptHash: paramCbor length " ++ show (BS.length paramCbor)
             ++ " exceeds single-chunk limit (255); not supported by TOA v1")
  | otherwise =
      let chunkLen  = BS.singleton (fromIntegral (BS.length paramCbor))
          chunkTerm = BS.singleton 0x00
          chunked   = chunkLen `BS.append` paramCbor `BS.append` chunkTerm
          flatBody  = flatPrefixToaV1 `BS.append` chunked `BS.append` flatSuffixToaV1
          hdr       = cborByteStringHeader (BS.length flatBody)
          applied   = hdr `BS.append` flatBody
      in hashToBytes (hashWith @Blake2b_224 id (BS.cons 0x03 applied))
  where
    paramCbor = serialiseToaParams toaVersion policyId assetName
