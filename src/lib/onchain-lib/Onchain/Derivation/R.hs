{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Canonical byte-level TOA v1 address-derivation function R, executable
-- on-chain using only standard Plutus V3 builtins: 'appendByteString',
-- 'consByteString', 'lengthOfByteString', 'serialiseData', 'blake2b_224'.
--
-- The byte-level constants 'flatPrefixToaV1' (528 B) and 'flatSuffixToaV1'
-- (1 B) are loaded at compile time from the canonical binary artifacts
-- via 'embedFile'. They are the same bytes the offchain mirror
-- @TxBuilding.Toa.DerivationR@ embeds — single source of truth: the .bin
-- files under @validators/@.
--
-- The recipe reproduces the flat-encoded UPLC layout empirically verified
-- by @toa-verify-reconstruction@:
--
-- > applied = CBOR_HDR(n) || FLAT_PREFIX || chunked(paramCbor) || FLAT_SUFFIX
-- > chunked(b) = consByteString(len(b), b) `appendByteString` consByteString(0x00, "")
--
-- where @len(b) <= 255@ (always true for TOA v1: max @paramCbor@ ~68 B).
--
-- See TOA CIP §"Address Derivation".
module Onchain.Derivation.R
  ( toaScriptHash
  , flatPrefixToaV1
  , flatSuffixToaV1
  ) where

import qualified Data.ByteString    as BS
import           Data.FileEmbed     (embedFile)
import           PlutusTx.Builtins  (mkB, mkConstr, mkI, serialiseData)
import qualified PlutusTx.Builtins  as Builtins
import           PlutusTx.Prelude

-- | Invariant 528-byte prefix of the flat-encoded TOA v1 applied program.
flatPrefixToaV1 :: BuiltinByteString
flatPrefixToaV1 =
  Builtins.toBuiltin
    (($(embedFile "validators/FLAT_PREFIX_TOA_V1.bin")) :: BS.ByteString)

-- | Invariant 1-byte suffix (@0x01@) of the flat-encoded TOA v1 applied
-- program.
flatSuffixToaV1 :: BuiltinByteString
flatSuffixToaV1 =
  Builtins.toBuiltin
    (($(embedFile "validators/FLAT_SUFFIX_TOA_V1.bin")) :: BS.ByteString)

emptyBs :: BuiltinByteString
emptyBs = ""

-- | CBOR major-type-2 (bytestring) length-prefix header for length @n@.
-- For TOA v1 the realisable flat_body sizes always hit the 0x59 + 2-byte
-- branch; the function covers the full RFC 8949 §4.2.1 spec for any uint
-- length up to 2^32 - 1, which suffices for any practical use.
{-# INLINABLE cborByteStringHeader #-}
cborByteStringHeader :: Integer -> BuiltinByteString
cborByteStringHeader n
  | n <= 23      = consByteString (0x40 + n) emptyBs
  | n <= 255     = consByteString 0x58 (consByteString n emptyBs)
  | n <= 65535   = consByteString 0x59
                     ( consByteString (divide n 256)
                       (consByteString (modulo n 256) emptyBs))
  | otherwise    = consByteString 0x5a
                     ( consByteString (divide n 16777216)
                       (consByteString (modulo (divide n 65536) 256)
                         (consByteString (modulo (divide n 256) 256)
                           (consByteString (modulo n 256) emptyBs))))

{-# INLINABLE toaScriptHash #-}
toaScriptHash
  :: Integer            -- ^ toa_version
  -> BuiltinByteString  -- ^ policy_id (28 bytes)
  -> BuiltinByteString  -- ^ asset_name (0..32 bytes)
  -> BuiltinByteString  -- ^ 28-byte script hash
toaScriptHash toaVersion policyId assetName =
  let paramCbor = serialiseData (mkConstr 0 [mkI toaVersion, mkB policyId, mkB assetName])
      paramLen  = lengthOfByteString paramCbor
      chunked   = consByteString paramLen paramCbor
                     `appendByteString` consByteString 0x00 emptyBs
      flatBody  = flatPrefixToaV1
                     `appendByteString` chunked
                     `appendByteString` flatSuffixToaV1
      hdr       = cborByteStringHeader (lengthOfByteString flatBody)
      applied   = hdr `appendByteString` flatBody
  in  blake2b_224 (consByteString 0x03 applied)
