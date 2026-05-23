{-# LANGUAGE LambdaCase #-}

-- | Pure conversions and datum helpers used by TOA skeletons, the vector exe,
-- and the @/toa/utxos@ handler.
--
-- Atlas already exports 'unitDatum' for PlutusData @Constr 0 []@ — the
-- wallet-sweep convention from the TOA CIP §"Datum and redeemer schema".
-- We re-use it; this module exposes only what Atlas doesn't.
module TxBuilding.Toa.Conversions
  ( paramsCborBytes
  , outDatumCborBytes
  , outDatumHashBytes
  ) where

import Cardano.Api qualified as Api
import Codec.CBOR.Encoding qualified as CBORE
import Codec.CBOR.Write qualified as CBOR
import Data.ByteString (ByteString)
import GeniusYield.Types
import Onchain.Protocol.Types (TOAParamsV1 (..))
import Utils (currencySymbolBytes, rawBytes, tokenNameBytes)

-- | PlutusData CBOR of a 'TOAParamsV1' in **RFC 8949 §4.2.1 canonical form**
-- (definite-length items, smallest-form integers). The CIP requires this:
-- see @TOA CIP §"Canonical Parameter Encoding"@.
--
-- This is the canonical PlutusData byte representation of
-- @Constr 0 [uint, bytes28, bytes(0..32)]@. Not used in address derivation
-- (which hashes UPLC, not this CBOR), but committed in @test-vectors/toa-v1.json@
-- under @params_cbor_hex@ and exposed by the @/toa/derive/trace@ endpoint, so
-- byte stability matters for cross-implementation verification.
paramsCborBytes :: TOAParamsV1 -> ByteString
paramsCborBytes = CBOR.toStrictByteString . paramsCanonicalEncoding

paramsCanonicalEncoding :: TOAParamsV1 -> CBORE.Encoding
paramsCanonicalEncoding TOAParamsV1 {toaVersion, toaPolicyId, toaAssetName} =
  CBORE.encodeTag 121                                -- PlutusData Constr 0 tag (#6.121)
    <> CBORE.encodeListLen 3                         -- definite-length array of 3
    <> CBORE.encodeInteger toaVersion                -- canonical uint
    <> CBORE.encodeBytes (currencySymbolBytes toaPolicyId)
    <> CBORE.encodeBytes (tokenNameBytes toaAssetName)

-- | Raw CBOR bytes of an inline datum, suitable for hex-encoding into a wire
-- response. 'Nothing' for hash-only and absent outputs.
outDatumCborBytes :: GYOutDatum -> Maybe ByteString
outDatumCborBytes = \case
  GYOutDatumInline d -> Just (Api.serialiseToCBOR (datumToApi' d))
  GYOutDatumHash _   -> Nothing
  GYOutDatumNone     -> Nothing

-- | Raw 32-byte blake2b_256 datum hash. Computed via 'hashDatum' for inline
-- datums and echoed from the ledger for hash-only outputs. 'Nothing' only for
-- 'GYOutDatumNone'.
outDatumHashBytes :: GYOutDatum -> Maybe ByteString
outDatumHashBytes = \case
  GYOutDatumInline d -> Just (rawHash (hashDatum d))
  GYOutDatumHash  h  -> Just (rawHash h)
  GYOutDatumNone     -> Nothing
  where
    rawHash = rawBytes . datumHashToApi
