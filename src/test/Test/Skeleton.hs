{-# LANGUAGE DataKinds #-}

-- | Reusable @GYTxSkeleton@ fragments for the TOA validator-scenario suite.
--
-- The four helpers below cover ~95% of the boilerplate that the scenarios
-- in 'UnitTests.Validator.Scenarios' would otherwise repeat verbatim:
-- spend a wallet UTxO with a key witness, spend a TOA UTxO with the per-NFT
-- script inlined, pay a value to an address with no datum, pay a value with
-- an inline datum.
module Test.Skeleton
  ( spendWalletKey
  , spendToaInlined
  , spendToaInlinedWithDatum
  , payNoDatum
  , payInlineDatum
  ) where

import GeniusYield.TxBuilder (GYTxSkeleton, mustHaveInput, mustHaveOutput)
import GeniusYield.Types
import Onchain.Protocol.Types (TOAParamsV1)
import TxBuilding.Toa.Validator (toaV1Script)

-- | Spend a wallet-owned UTxO using a key witness.
spendWalletKey :: GYUTxO -> GYTxSkeleton 'PlutusV3
spendWalletKey u =
  mustHaveInput
    GYTxIn
      { gyTxInTxOutRef = utxoRef u
      , gyTxInWitness  = GYTxInWitnessKey
      }

-- | Spend a TOA UTxO using the per-NFT applied script inlined into the tx.
-- Suitable for deposits with inline (or absent) datums; for hash-only
-- deposits use 'spendToaInlinedWithDatum'.
spendToaInlined :: TOAParamsV1 -> GYUTxO -> GYTxSkeleton 'PlutusV3
spendToaInlined params u =
  mustHaveInput
    GYTxIn
      { gyTxInTxOutRef = utxoRef u
      , gyTxInWitness  =
          GYTxInWitnessScript
            (GYBuildPlutusScriptInlined @'PlutusV3 (toaV1Script params))
            Nothing
            (redeemerFromPlutusData ())
      }

-- | Spend a TOA UTxO when the deposit was hash-only and we have to supply
-- the datum preimage explicitly. Atlas's pre-submission checks require it
-- even though the TOA validator itself ignores the datum.
spendToaInlinedWithDatum ::
  TOAParamsV1 -> GYUTxO -> GYDatum -> GYTxSkeleton 'PlutusV3
spendToaInlinedWithDatum params u d =
  mustHaveInput
    GYTxIn
      { gyTxInTxOutRef = utxoRef u
      , gyTxInWitness  =
          GYTxInWitnessScript
            (GYBuildPlutusScriptInlined @'PlutusV3 (toaV1Script params))
            (Just d)
            (redeemerFromPlutusData ())
      }

-- | Pay a value to an address with no datum and no reference script.
payNoDatum :: GYAddress -> GYValue -> GYTxSkeleton 'PlutusV3
payNoDatum addr v =
  mustHaveOutput
    GYTxOut
      { gyTxOutAddress = addr
      , gyTxOutDatum   = Nothing
      , gyTxOutValue   = v
      , gyTxOutRefS    = Nothing
      }

-- | Pay a value to an address with an inline datum.
payInlineDatum :: GYAddress -> GYDatum -> GYValue -> GYTxSkeleton 'PlutusV3
payInlineDatum addr d v =
  mustHaveOutput
    GYTxOut
      { gyTxOutAddress = addr
      , gyTxOutDatum   = Just (d, GYTxOutUseInlineDatum @'PlutusV3)
      , gyTxOutValue   = v
      , gyTxOutRefS    = Nothing
      }
