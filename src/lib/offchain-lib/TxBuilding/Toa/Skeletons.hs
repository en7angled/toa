{-# LANGUAGE DataKinds #-}

-- | Reusable 'GYTxSkeleton' fragments for TOA tx building.
--
-- Three of the four helpers are pure: they assemble a 'GYTxIn'/'GYTxOut'
-- from caller-supplied data and return a 'GYTxSkeleton ''PlutusV3' without
-- consulting the chain. 'txMustSpendNFT' is the one exception — it queries
-- for the wallet UTxO holding the controlling NFT and is therefore
-- monadic.
module TxBuilding.Toa.Skeletons
  ( txMustPayValueToAddress
  , txMustPayValueToAddressWithDatum
  , txMustSpendNFT
  , txMustSpendUTXOsFromScript
  ) where

import GeniusYield.TxBuilder
import GeniusYield.Types
import TxBuilding.Toa.Query (getUTxOWithNFT)

txMustPayValueToAddress :: GYAddress -> GYValue -> GYTxSkeleton 'PlutusV3
txMustPayValueToAddress recipient gyValue =
  mustHaveOutput
    GYTxOut
      { gyTxOutAddress = recipient,
        gyTxOutDatum = Nothing,
        gyTxOutValue = gyValue,
        gyTxOutRefS = Nothing
      }

txMustPayValueToAddressWithDatum :: GYAddress -> GYValue -> GYTxSkeleton 'PlutusV3
txMustPayValueToAddressWithDatum recipient gyValue =
  mustHaveOutput
    GYTxOut
      { gyTxOutAddress = recipient,
        gyTxOutDatum = Just (unitDatum, GYTxOutUseInlineDatum),
        gyTxOutValue = gyValue,
        gyTxOutRefS = Nothing
      }

txMustSpendNFT :: (GYTxUserQueryMonad m) => GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txMustSpendNFT nftAc = do
  nftUtxo <- getUTxOWithNFT nftAc
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = utxoRef nftUtxo,
          gyTxInWitness = GYTxInWitnessKey
        }

txMustSpendUTXOsFromScript ::
  [(GYTxOutRef, Maybe (GYDatum, GYTxOutUseInlineDatum 'PlutusV3))] ->
  GYScript 'PlutusV3 ->
  GYTxSkeleton 'PlutusV3
txMustSpendUTXOsFromScript utxosrefs gyValidator =
  mconcat
    [ mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoref,
            gyTxInWitness =
              GYTxInWitnessScript
                (GYBuildPlutusScriptInlined $ validatorToScript gyValidator)
                (fst <$> mDatum)
                unitRedeemer
          }
      | (utxoref, mDatum) <- utxosrefs
    ]
