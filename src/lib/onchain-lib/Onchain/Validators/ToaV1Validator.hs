{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Canonical TOA v1 parameterised validator (Plutus V3).
--
-- For an asset class @ac = (toaPolicyId, toaAssetName)@, a TOA-spending
-- transaction is valid iff:
--
--   * @valueOf(txInfoMint, ac) == 0@                   (T0)
--   * @sumSpentInputs(ac) == 1@                        (T1, regular inputs only)
--   * @sumOutputs(ac) == 1@                            (T2)
--
-- The validator deliberately ignores the spending UTxO's datum: per CIP-69 the
-- datum is delivered via 'ScriptInfo' for V3 spends, and per the TOA CIP
-- §"Datum and redeemer schema" a TOA spend MUST NOT fail because the datum
-- is absent, hashed, or non-unit. Likewise the redeemer is ignored.
--
-- The summation is done over @txInfoInputs@ — the regular spent inputs —
-- NOT @txInfoReferenceInputs@. This is what defeats the reference-input
-- attack (CIP §Validator Rules and §"Why total quantity, not …").
--
-- This module imports from @PlutusLedgerApi.Data.V3@ (data-backed) rather
-- than @PlutusLedgerApi.V3@ (SoP). The data-backed API keeps un-read
-- 'TxInfo' fields as opaque 'BuiltinData' instead of eagerly decoding them
-- into sums-of-products, which is materially cheaper for V3 'ScriptContext'
-- values where the validator inspects only 3 of 16 'TxInfo' fields. See the
-- Plutus team's guidance at
-- https://plutus.cardano.intersectmbo.org/docs/working-with-scripts/optimizing-scripts-with-asData
-- for background. Net-mint of the controlling AC is checked via a direct
-- 'AssocMap' lookup on the underlying mint map, avoiding the two-pass
-- 'mintValueMinted'/'mintValueBurned' materialisation that the SoP-style
-- implementation would require.
module Onchain.Validators.ToaV1Validator
  ( toaV1Lambda,
    toaV1Untyped,
    toaV1Compiled,
    toaV1ApplyConstantData,
  )
where

import Control.Monad.Except (runExcept)
import Onchain.Protocol.Types (TOAParamsV1 (..))
import Onchain.Utils (mkUntypedLambda)
import PlutusCore qualified as PLC
import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V1.Data.Value (valueOf)
import PlutusLedgerApi.V1.Value qualified as SopValue
import PlutusTx
import PlutusTx.Data.AssocMap qualified as DAssocMap
import PlutusTx.Data.List qualified as DList
import PlutusTx.Maybe qualified as Data.Maybe
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC
import Prelude qualified as Haskell

-- The applied script's parameter type still references the SoP V1
-- 'CurrencySymbol' and 'TokenName' (via 'Onchain.Protocol.Types'). The wire
-- 'Data' representation is identical to the data-backed variants — both are
-- @B bytes@ — so we re-wrap into the data-backed newtypes before any lookup
-- on the data-backed mint map or value.
{-# INLINEABLE sopToDataCs #-}
sopToDataCs :: SopValue.CurrencySymbol -> CurrencySymbol
sopToDataCs (SopValue.CurrencySymbol bs) = CurrencySymbol bs

{-# INLINEABLE sopToDataTn #-}
sopToDataTn :: SopValue.TokenName -> TokenName
sopToDataTn (SopValue.TokenName bs) = TokenName bs

{-# INLINEABLE toaV1Lambda #-}
toaV1Lambda :: TOAParamsV1 -> ScriptContext -> Bool
toaV1Lambda
  TOAParamsV1 {toaPolicyId, toaAssetName}
  (ScriptContext TxInfo {txInfoInputs, txInfoOutputs, txInfoMint} _redeemer scriptInfo) =
    case scriptInfo of
      SpendingScript _ownRef _mDatum ->
        let cs = sopToDataCs toaPolicyId
            tn = sopToDataTn toaAssetName
            sumIn =
              DList.foldr
                (\TxInInfo {txInInfoResolved = TxOut {txOutValue = v}} acc -> acc + valueOf v cs tn)
                0
                txInfoInputs
            sumOut =
              DList.foldr
                (\TxOut {txOutValue = v} acc -> acc + valueOf v cs tn)
                0
                txInfoOutputs
            -- Net mint of the controlling AC must be 0. Walk the underlying
            -- @Map CurrencySymbol (Map TokenName Integer)@ once instead of
            -- materialising 'mintValueMinted'/'mintValueBurned' twice.
            mintAC =
              case DAssocMap.lookup cs (mintValueToMap txInfoMint) of
                Nothing -> 0
                Just inner ->
                  Data.Maybe.fromMaybe 0 (DAssocMap.lookup tn inner)
         in traceIfFalse "T0" (mintAC == 0)
              && traceIfFalse "T1" (sumIn == 1)
              && traceIfFalse "T2" (sumOut == 1)
      _ -> traceError "T3" -- Wrong script purpose (T3)

-- | The validator's untyped form, parameterised by a 'BuiltinData' rather
-- than by a Plinth SoP 'TOAParamsV1'. The parameter is delivered to the
-- applied script as a UPLC @Constant Data@ term — the same shape that
-- @cardano-api@'s @applyArguments@ produces and that @PlutusLedgerApi@'s
-- pre-pin @applyArguments@ used. The internal 'unsafeFromBuiltinData'
-- recovers the 'TOAParamsV1' record before running 'toaV1Lambda'.
--
-- Switching the parameter to 'BuiltinData' is what makes canonical
-- byte-level address derivation (R) feasible: R reconstructs applied bytes
-- from a fixed prefix\/suffix and the canonical CBOR of the parameter, which
-- only matches the @Constant Data@ application path. The previous
-- @TOAParamsV1 -> BuiltinData -> BuiltinUnit@ shape forced 'PlutusTx.liftCode'
-- to emit a Plinth SoP constructor value at compile time, which R cannot
-- reconstruct.
{-# INLINEABLE toaV1Untyped #-}
toaV1Untyped :: BuiltinData -> BuiltinData -> BuiltinUnit
toaV1Untyped paramsData = mkUntypedLambda (toaV1Lambda (PlutusTx.unsafeFromBuiltinData paramsData))

-- | Un-applied validator. The hash of its serialised UPLC is the
-- /template hash/ committed in the CIP.
toaV1Compiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
toaV1Compiled = $$(compile [||toaV1Untyped||])

-- | Apply the validator with a concrete 'TOAParamsV1' via the UPLC
-- @Constant Data@ path. The hash of the resulting serialised UPLC is the
-- per-NFT TOA script hash (payment credential).
--
-- This is the exact application path used by @cardano-api@'s
-- @applyArguments@ and Atlas's @applyParam@: deserialise the un-applied
-- program, build a UPLC argument program whose body is a single
-- @Constant Data@ term, and re-serialise the resulting @Apply@ node.
toaV1ApplyConstantData :: TOAParamsV1 -> SerialisedScript
toaV1ApplyConstantData params =
  let unapplied = serialiseCompiledCode toaV1Compiled
      prog@(UPLC.Program _ ver _) = uncheckedDeserialiseUPLC unapplied
      arg = UPLC.Program () ver (UPLC.Constant () (PLC.someValue (toData params)))
   in case runExcept (UPLC.applyProgram prog arg) of
        Left e -> Haskell.error ("toaV1ApplyConstantData: applyProgram failed: " Haskell.++ Haskell.show e)
        Right p -> serialiseUPLC p
