{-# LANGUAGE NoImplicitPrelude #-}

-- | Canonical TOA v1 parameter type.
--
-- Defines 'TOAParamsV1' — the single PlutusData argument the TOA v1 validator
-- is applied with. The serialisation is pinned by the CIP CDDL to
-- @Constr 0 [uint, bytes28, bytes 0..32]@ (PlutusData tag @#6.121@), so
-- 'makeIsDataIndexed' forces constructor index 0 and 'makeLift' lets the value
-- be spliced into the UPLC at compile time via 'PlutusTx.unsafeApplyCode'.
--
-- Keep this module Plinth-pure: no Aeson, no Atlas. The off-chain mirror with
-- JSON instances lives in 'DomainTypes.Core.Types'.
module Onchain.Protocol.Types
  ( TOAParamsV1 (..)
  , toaVersionV1
  ) where

import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName)
import PlutusTx (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude
import Prelude qualified

-- | Canonical TOA v1 parameter. Serialised as @Constr 0 [uint, bytes28, bytes 0..32]@.
data TOAParamsV1 = TOAParamsV1
  { toaVersion :: Integer
  , toaPolicyId :: CurrencySymbol
  , toaAssetName :: TokenName
  }
  deriving stock (Prelude.Show, Prelude.Eq)

makeIsDataIndexed ''TOAParamsV1 [('TOAParamsV1, 0)]
makeLift ''TOAParamsV1

-- | The standard revision pinned by this CIP.
{-# INLINEABLE toaVersionV1 #-}
toaVersionV1 :: Integer
toaVersionV1 = 1
