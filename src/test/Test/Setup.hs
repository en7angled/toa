{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared fixtures for the TOA validator-scenario CLB suite.
--
-- Provides:
--   * 'mintTestNft'           — mint N units of an asset under the always-true MP
--   * 'depositToToaUTxO'      — lock a value at the TOA address, return its 'GYTxOutRef'
--   * 'findOwnedUTxOWithAsset' — find a user-held UTxO containing a given asset
--   * 'getToaUTxOs'           — list all UTxOs at the TOA address
--   * 'paramsFor'             — build a 'TOAParamsV1' from the always-true MP + token name
module Test.Setup
  ( mintTestNft
  , depositToToaUTxO
  , findOwnedUTxOWithAsset
  , getToaUTxOs
  , paramsFor
  ) where

import Control.Monad (void)
import Data.List (find)
import GHC.Stack (HasCallStack)
import GeniusYield.Test.Clb (sendSkeleton')
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.Protocol.Types (TOAParamsV1 (..))
import Test.Policies (alwaysTrueCS, alwaysTrueMP, alwaysTrueMPId)
import TxBuilding.Toa.Address (toaAddress)

-- | Build a 'TOAParamsV1' from a token name (using the always-true policy as CS).
paramsFor :: GYTokenName -> TOAParamsV1
paramsFor tn = TOAParamsV1 1 alwaysTrueCS (tokenNameToPlutus tn)

-- | Network-id-scoped TOA address for a params value.
toaAddrIn :: (GYTxQueryMonad m) => TOAParamsV1 -> m GYAddress
toaAddrIn params = do
  nid <- networkId
  pure (toaAddress nid params)

-- | Mint @qty@ units of @(alwaysTrueCS, tn)@ payable to @user@. Returns the
-- asset class and the corresponding 'TOAParamsV1'.
mintTestNft ::
  (GYTxGameMonad m, HasCallStack) =>
  User ->
  GYTokenName ->
  Integer ->
  m (GYAssetClass, TOAParamsV1)
mintTestNft user tn qty = asUser user $ do
  let mp = GYMintScript @'PlutusV3 alwaysTrueMP
      redeemer = redeemerFromPlutusData ()
      mintSkel = mustMint mp redeemer tn qty
      ac = GYToken alwaysTrueMPId tn
      payOutput =
        mustHaveOutput
          GYTxOut
            { gyTxOutAddress = userChangeAddress user
            , gyTxOutDatum = Nothing
            , gyTxOutValue = valueSingleton ac qty <> valueFromLovelace 2_000_000
            , gyTxOutRefS = Nothing
            }
  void $ sendSkeleton' (mintSkel <> payOutput)
  pure (ac, paramsFor tn)

-- | Lock a UTxO at the TOA address. Returns its 'GYTxOutRef'.
depositToToaUTxO ::
  (GYTxGameMonad m, HasCallStack) =>
  User ->
  TOAParamsV1 ->
  GYValue ->
  Maybe (GYDatum, GYTxOutUseInlineDatum 'PlutusV3) ->
  m GYTxOutRef
depositToToaUTxO user params value mDatum = asUser user $ do
  addr <- toaAddrIn params
  let out =
        GYTxOut
          { gyTxOutAddress = addr
          , gyTxOutDatum = mDatum
          , gyTxOutValue = value
          , gyTxOutRefS = Nothing
          }
  void $ sendSkeleton' (mustHaveOutput out)
  -- Find the freshly-created UTxO by exact-value match. The TOA address starts
  -- empty in each scenario, so a value match is sufficient. Tests that deposit
  -- multiple UTxOs with the same value must use distinct values to disambiguate.
  utxos <- utxosToList <$> utxosAtAddress addr Nothing
  case find ((== value) . utxoValue) utxos of
    Just u -> pure (utxoRef u)
    Nothing -> error "Test.Setup.depositToToaUTxO: deposited UTxO not found at TOA address"

-- | Find a UTxO at the user's change address containing at least 1 of the given asset.
findOwnedUTxOWithAsset ::
  (GYTxGameMonad m, HasCallStack) =>
  User ->
  GYAssetClass ->
  m GYUTxO
findOwnedUTxOWithAsset user ac = asUser user $ do
  utxos <- utxosToList <$> utxosAtAddress (userChangeAddress user) Nothing
  case find (\u -> valueAssetClass (utxoValue u) ac > 0) utxos of
    Just u -> pure u
    Nothing -> error "Test.Setup.findOwnedUTxOWithAsset: asset not found in user's wallet"

-- | List all UTxOs at the TOA address for the given params.
getToaUTxOs ::
  (GYTxQueryMonad m) =>
  TOAParamsV1 ->
  m [GYUTxO]
getToaUTxOs params = do
  addr <- toaAddrIn params
  utxosToList <$> utxosAtAddress addr Nothing
