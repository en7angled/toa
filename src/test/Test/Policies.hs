{-# LANGUAGE DataKinds #-}

-- | Atlas wrapper for the test-only always-true minting policy.
module Test.Policies
  ( alwaysTrueMP
  , alwaysTrueMPId
  , alwaysTrueCS
  ) where

import GeniusYield.Types
import Onchain.Test.AlwaysTrueMP (alwaysTrueCompiled)
import PlutusLedgerApi.V1.Value (CurrencySymbol)

-- | Always-true Plutus V3 minting policy.
alwaysTrueMP :: GYScript 'PlutusV3
alwaysTrueMP = validatorFromPlutus alwaysTrueCompiled

-- | Atlas minting policy id (28-byte script hash).
alwaysTrueMPId :: GYMintingPolicyId
alwaysTrueMPId = mintingPolicyId alwaysTrueMP

-- | Plutus 'CurrencySymbol' for the always-true policy.
alwaysTrueCS :: CurrencySymbol
alwaysTrueCS = mintingPolicyIdToCurrencySymbol alwaysTrueMPId
