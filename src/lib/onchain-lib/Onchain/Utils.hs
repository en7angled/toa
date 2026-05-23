{-# LANGUAGE NoImplicitPrelude #-}

-- | On-chain utility helpers shared by TOA validators.
module Onchain.Utils
  ( mkUntypedLambda
  ) where

import PlutusLedgerApi.Data.V3 (ScriptContext)
import PlutusTx.Builtins
import PlutusTx.IsData.Class (unsafeFromBuiltinData)
import PlutusTx.Prelude

-- | Converts a typed validator lambda to the untyped @BuiltinData -> BuiltinUnit@
-- form expected by Plutus V3.
--
-- Uses 'unsafeFromBuiltinData' for the 'ScriptContext' decode. Per the Plinth
-- optimisation guide, the unchecked decoder is the right choice when a
-- malformed input is unrecoverable — a malformed 'ScriptContext' cannot lead
-- to a meaningful spend, and the safe variant's 'Maybe' case-split is wasted
-- work for every invocation.
{-# INLINEABLE mkUntypedLambda #-}
mkUntypedLambda ::
  (ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinUnit)
mkUntypedLambda f c = check (f (unsafeFromBuiltinData c))
