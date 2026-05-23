-- | Off-chain wrapper around the TOA v1 Plinth validator.
--
-- Two paths:
--
--   * /Applied/ — per-TOA. 'toaV1Script' produces a @GYScript 'PlutusV3@ from a
--     concrete 'TOAParamsV1', and 'toaV1ScriptHash' is the payment credential
--     of the derived address. Atlas's 'validatorHash' already implements
--     @blake2b_224(0x03 || ledger_serialised_plutus_script(applied))@.
--
--   * /Un-applied template/ — single value committed in the CIP. Atlas's
--     'validatorFromPlutus' expects @CompiledCode (BuiltinData -> BuiltinUnit)@,
--     which the un-applied template is /not/, so we compute the template hash
--     directly per the CIP formula @blake2b_224(0x03 || unapplied_script_bytes)@
--     using 'cardano-crypto-class'.
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
import Onchain.Validators.ToaV1Validator (toaV1Apply, toaV1Compiled)
import PlutusLedgerApi.V3 (serialiseCompiledCode)

-- | Applied per-TOA script.
toaV1Script :: TOAParamsV1 -> GYScript 'PlutusV3
toaV1Script params = validatorFromPlutus (toaV1Apply params)

-- | Per-TOA payment-credential hash.
toaV1ScriptHash :: TOAParamsV1 -> GYScriptHash
toaV1ScriptHash = validatorHash . toaV1Script

-- | Raw applied-script CBOR bytes (post-@apply_params@, pre-hash).
-- Same value that 'validatorFromPlutus' feeds into Atlas's 'validatorHash',
-- exposed directly so the HTTP API can echo it back to clients.
toaV1ApplyBytes :: TOAParamsV1 -> ByteString
toaV1ApplyBytes = SBS.fromShort . serialiseCompiledCode . toaV1Apply

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
