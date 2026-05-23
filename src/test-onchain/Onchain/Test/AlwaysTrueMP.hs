{-# LANGUAGE NoImplicitPrelude #-}

-- | Test-only always-true minting policy.
--
-- Used by the CLB validator-scenario suite to mint controlling NFTs without
-- imposing any policy constraint. NOT for production: this policy lets any
-- party mint or burn any quantity at any time (i.e. @KnownOpen@ in CIP terms).
module Onchain.Test.AlwaysTrueMP
  ( alwaysTrueUntyped
  , alwaysTrueCompiled
  ) where

import PlutusTx
import PlutusTx.Prelude

{-# INLINEABLE alwaysTrueUntyped #-}
alwaysTrueUntyped :: BuiltinData -> BuiltinUnit
alwaysTrueUntyped _ = check True

alwaysTrueCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueCompiled = $$(compile [|| alwaysTrueUntyped ||])
