-- | Atlas read-only queries used by the TOA HTTP API.
--
-- These helpers run inside any 'GYTxQueryMonad' and do not assume CLB or a
-- live provider; the caller supplies the runner ('runGYTxQueryMonadIO' for
-- Maestro, etc.).
module TxBuilding.Toa.Query
  ( getUTxOWithNFT
  , getUTxOsAtAddressCoveringValue
  ) where

import Data.Maybe (mapMaybe)
import GeniusYield.TxBuilder.Class
import GeniusYield.TxBuilder.Errors
import GeniusYield.Types
import TxBuilding.Exceptions

-- | Return the single UTxO from a list, or throw a domain error for empty or multiple.
requireSingleUtxo ::
  (GYTxQueryMonad m) =>
  [GYUTxO] ->
  m GYUTxO
requireSingleUtxo utxos = case utxos of
  [u] -> return u
  [] -> throwError (GYApplicationException NFTNotFound)
  _ -> throwError (GYApplicationException MultipleUtxosFound)

getUTxOWithNFT :: (GYTxQueryMonad m) => GYAssetClass -> m GYUTxO
getUTxOWithNFT gyAC = do
  nonAdaToken <- maybe (throwError (GYApplicationException InvalidAssetClass)) return (nonAdaTokenFromAssetClass gyAC)
  utxos <- utxosWithAsset nonAdaToken
  requireSingleUtxo (utxosToList utxos)

-- | Select UTxOs at an address until their combined value covers the requested value.
-- Returns the selected UTxO refs paired with their datum shape (or 'Nothing' for
-- no-datum outputs) and the remaining value after subtracting the request.
-- Hash-only datums are skipped: the ledger requires the datum body in the witness
-- set at spend, and we cannot resolve a bare hash here.
getUTxOsAtAddressCoveringValue ::
  (GYTxQueryMonad m) =>
  GYAddress ->
  GYValue ->
  m ([(GYTxOutRef, Maybe (GYDatum, GYTxOutUseInlineDatum 'PlutusV3))], GYValue)
getUTxOsAtAddressCoveringValue address requested = do
  utxosWithDatums <- mapMaybe utxoWithDatum . utxosToList <$> utxosAtAddress address Nothing
  let (selectedUtxos, selectedValue) = selectCoveringUTxOs requested utxosWithDatums
  if selectedValue `valueGreaterOrEqual` requested
    then return (selectedUtxos, selectedValue `valueMinus` requested)
    else throwError (GYApplicationException (InsufficientToaValue requested selectedValue))

utxoWithDatum :: GYUTxO -> Maybe (GYUTxO, Maybe (GYDatum, GYTxOutUseInlineDatum 'PlutusV3))
utxoWithDatum utxo = case utxoOutDatum utxo of
  GYOutDatumInline d -> Just (utxo, Just (d, GYTxOutUseInlineDatum))
  GYOutDatumNone -> Just (utxo, Nothing)
  GYOutDatumHash _ -> Nothing

selectCoveringUTxOs ::
  GYValue ->
  [(GYUTxO, Maybe (GYDatum, GYTxOutUseInlineDatum 'PlutusV3))] ->
  ([(GYTxOutRef, Maybe (GYDatum, GYTxOutUseInlineDatum 'PlutusV3))], GYValue)
selectCoveringUTxOs requested = go [] mempty
 where
  go selected selectedValue remaining
    | selectedValue `valueGreaterOrEqual` requested = (reverse selected, selectedValue)
    | otherwise = case remaining of
        [] -> (reverse selected, selectedValue)
        (utxo, mDatum) : rest ->
          go
            ((utxoRef utxo, mDatum) : selected)
            (selectedValue <> utxoValue utxo)
            rest
