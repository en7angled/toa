-- | Off-chain wrapper around the TOA v1 Plinth validator.
--
-- Two paths:
--
--   * /Applied/ — per-TOA. 'toaV1Script' produces a @GYScript 'PlutusV3@ from a
--     concrete 'TOAParamsV1' via the UPLC @Constant Data@ application path
--     (the same path used by @cardano-api@'s @applyArguments@). The hash of
--     this script is the payment credential of the derived address.
--
--   * /Un-applied template/ — single value committed in the CIP. The un-applied
--     UPLC has shape @CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)@
--     which is /not/ what Atlas's 'validatorFromPlutus' expects, so we compute
--     the template hash directly per the CIP formula
--     @blake2b_224(0x03 || unapplied_script_bytes)@ using 'cardano-crypto-class'.
module TxBuilding.Toa.Validator
  ( -- * Applied path (per-TOA address)
    toaV1Script,
    toaV1ScriptHash,
    toaV1ApplyBytes,

    -- * Un-applied template path (committed CIP artifact)
    toaV1UnappliedBytes,
    toaV1UnappliedHashBytes,
    writeUnappliedBytes,
  )
where

import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Crypto.Hash.Class (hashToBytes, hashWith)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import GeniusYield.Types
import Onchain.Protocol.Types (TOAParamsV1)
import Onchain.Validators.ToaV1Validator (toaV1ApplyConstantData, toaV1Compiled)
import PlutusLedgerApi.V3 (serialiseCompiledCode)

-- | Applied per-TOA script. Construction goes through the UPLC
-- @Constant Data@ application path so the resulting bytes match what
-- @cardano-api@\/wallet tooling would produce.
toaV1Script :: TOAParamsV1 -> GYScript 'PlutusV3
toaV1Script = scriptFromSerialisedScript . toaV1ApplyConstantData

-- | Per-TOA payment-credential hash.
toaV1ScriptHash :: TOAParamsV1 -> GYScriptHash
toaV1ScriptHash = validatorHash . toaV1Script

-- | Raw applied-script CBOR bytes (post-@apply_params@, pre-hash).
-- Same value that 'validatorHash' (via 'toaV1Script') ultimately hashes,
-- exposed directly so the HTTP API can echo it back to clients.
toaV1ApplyBytes :: TOAParamsV1 -> ByteString
toaV1ApplyBytes = SBS.fromShort . toaV1ApplyConstantData

-- | Raw un-applied UPLC bytes (single CBOR-bytestring wrapping the flat UPLC
-- program). This is the byte sequence committed at @validators/ToaV1.uplc@.
toaV1UnappliedBytes :: ByteString
toaV1UnappliedBytes = SBS.fromShort (serialiseCompiledCode toaV1Compiled)

-- | Template hash committed in the CIP. 28 bytes.
toaV1UnappliedHashBytes :: ByteString
toaV1UnappliedHashBytes =
  hashToBytes (hashWith @Blake2b_224 id (BS.cons 0x03 toaV1UnappliedBytes))

-- | Write the raw un-applied UPLC blob to disk. Not a TextEnvelope JSON — the
-- TextEnvelope footgun is called out in the TOA CIP §"Address derivation algorithm".
writeUnappliedBytes :: FilePath -> IO ()
writeUnappliedBytes fp = BS.writeFile fp toaV1UnappliedBytes
