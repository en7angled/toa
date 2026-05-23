{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CLB validator scenarios for TOA v1.
--
-- Exercises every rule in the TOA CIP §"Validator Rules":
--   T0 — no mint or burn of the controlling asset class
--   T1 — exactly 1 unit of the asset class in spent (non-reference) inputs
--   T2 — exactly 1 unit of the asset class in outputs
--
-- Trace codes expected on failure are documented in each scenario's test name.
-- 'mustFail' itself is binary; trace strings are visible via
-- @cabal test --test-show-details=streaming@.
module UnitTests.Validator.Scenarios
  ( scenarioTests
  ) where

import Control.Monad (void)
import GHC.Stack (HasCallStack)
import GeniusYield.Test.Clb (GYTxMonadClb, mkTestFor, mustFail, sendSkeleton')
import GeniusYield.Test.Utils (TestInfo (..), Wallets (..))
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Policies (alwaysTrueMP)
import Test.Setup (depositToToaUTxO, findOwnedUTxOWithAsset, mintTestNft, paramsFor)
import Test.Skeleton (payInlineDatum, payNoDatum, spendToaInlined, spendToaInlinedWithDatum, spendWalletKey)
import Test.Tasty (TestTree, testGroup)
import TxBuilding.Toa.Address (toaAddress)

scenarioTests :: TestTree
scenarioTests =
  testGroup
    "Validator Scenarios"
    [ mkTestFor "S1 positive_baseline (PASS)" s1PositiveBaseline
    , mkTestFor "S2 non_unique_input_fails (mustFail T1; T2-only impossible)" s2NonUniqueInputFails
    , mkTestFor "S4 mint_in_spend_fails (mustFail T0)" s4MintInSpendFails
    , mkTestFor "S5 burn_in_spend_fails (mustFail T0)" s5BurnInSpendFails
    , mkTestFor "S6 reference_input_attack_fails (mustFail T1)" s6ReferenceInputAttackFails
    , mkTestFor "S7 many_toa_utxos_one_nft_carry_through (PASS)" s7ManyToaUtxos
    , mkTestFor "S8 self_deposit_permissionless_spend (PASS)" s8SelfDepositPermissionlessSpend
    , mkTestFor "S9a non_conforming: no_datum_spendable (PASS)" (s9NonConforming Nothing)
    , mkTestFor "S9b non_conforming: hash_only_spendable (PASS)" (s9NonConforming (Just (datumFromPlutusData (), GYTxOutDontUseInlineDatum)))
    , mkTestFor "S9c non_conforming: non_unit_inline_spendable (PASS)" (s9NonConforming (Just (junkDatum, GYTxOutUseInlineDatum @'PlutusV3)))
    , mkTestFor "S10 wrong_asset_name_same_policy_fails (mustFail T1)" s10WrongAssetNameFails
    , mkTestFor "S11 nft_redeposited_at_toa (PASS)" s11NftRedepositedAtToa
    , mkTestFor "S12 nft_regular_input_w2_owner (PASS)" s12NftRegularInputW2Owner
    , mkTestFor "S13 many_toa_reference_attack_fails (mustFail T1)" s13ManyToaReferenceAttackFails
    ]
  where
    -- A non-unit PlutusData datum (serialises to @I 42@, not @Constr 0 []@).
    -- Used by S9c to verify the validator accepts non-unit inline datums.
    junkDatum :: GYDatum
    junkDatum = datumFromPlutusData (42 :: Integer)

-------------------------------------------------------------------------------
-- S1: positive baseline
-------------------------------------------------------------------------------

-- | S1 — Mint 1 NFT to W1; W1 deposits 5 ADA at the TOA; W1 spends the TOA
-- carrying the NFT through to their own wallet. PASS.
s1PositiveBaseline :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s1PositiveBaseline TestInfo {testWallets = Wallets {w1}} = do
  (nftAC, params) <- mintTestNft w1 "TOA-NFT-001" 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue Nothing
  asUser w1 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    nftUtxo <- findOwnedUTxOWithAsset w1 nftAC
    w1Addr  <- ownChangeAddress
    let spendSkel =
          spendToaInlined params toaUtxo
          <> spendWalletKey nftUtxo
          <> payNoDatum w1Addr (valueSingleton nftAC 1)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S2: non-unique input (collapsed S2 + S3)
-------------------------------------------------------------------------------

-- | S2 — Mint 2 units of the asset to W1. Spend tx consumes both NFT UTxOs
-- (sumIn = 2) plus the TOA UTxO. Validator's T1 short-circuits before T2
-- (`&&` left-to-right), so we see the T1 trace. mustFail.
--
-- Pure T2-only isolation is structurally impossible: with @mint = 0@ ledger
-- conservation forces @sumIn = sumOut@. The validator's three checks are
-- jointly exercised across S1/S2/S4/S5/S6; no actual coverage gap.
s2NonUniqueInputFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s2NonUniqueInputFails TestInfo {testWallets = Wallets {w1}} = do
  -- Mint TWO units of the same asset to W1.
  (nftAC, params) <- mintTestNft w1 "TOA-NFT-S2" 2
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue Nothing
  mustFail $ asUser w1 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    -- The 2 minted units live in one UTxO (Atlas coalesces same-asset outputs).
    -- Consuming that UTxO yields sumIn = 2 for the asset class.
    nftUtxo <- findOwnedUTxOWithAsset w1 nftAC
    w1Addr  <- ownChangeAddress
    let spendSkel =
          spendToaInlined params toaUtxo
          <> spendWalletKey nftUtxo
          <> payNoDatum w1Addr (valueSingleton nftAC 2)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S4: mint additional units in the spending tx
-------------------------------------------------------------------------------

-- | S4 — Mint 1 NFT to W1, deposit 5 ADA at TOA, then in the spending tx
-- /mint one more unit/ of the same @(policyId, assetName)@. The TOA validator
-- sees @mintedAC == 1@, so T0 fires. mustFail.
s4MintInSpendFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s4MintInSpendFails TestInfo {testWallets = Wallets {w1}} = do
  let tn = "TOA-NFT-S4"
  (nftAC, params) <- mintTestNft w1 tn 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue Nothing
  mustFail $ asUser w1 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    nftUtxo <- findOwnedUTxOWithAsset w1 nftAC
    w1Addr  <- ownChangeAddress
    let mp        = GYMintScript @'PlutusV3 alwaysTrueMP
        extraMint = mustMint mp (redeemerFromPlutusData ()) tn 1
        spendSkel =
          spendToaInlined params toaUtxo
          <> spendWalletKey nftUtxo
          <> extraMint
          <> payNoDatum w1Addr (valueSingleton nftAC 2)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S5: burn the NFT in the spending tx
-------------------------------------------------------------------------------

-- | S5 — Mint 1 NFT to W1, deposit 5 ADA at TOA, then in the spending tx
-- /burn the NFT/ (mint @-1@). Validator sees @burnedAC == 1@, T0 fires.
-- mustFail.
s5BurnInSpendFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s5BurnInSpendFails TestInfo {testWallets = Wallets {w1}} = do
  let tn = "TOA-NFT-S5"
  (nftAC, params) <- mintTestNft w1 tn 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue Nothing
  mustFail $ asUser w1 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    nftUtxo <- findOwnedUTxOWithAsset w1 nftAC
    let mp = GYMintScript @'PlutusV3 alwaysTrueMP
        burn = mustMint mp (redeemerFromPlutusData ()) tn (-1)
        -- no NFT output; Atlas balancer absorbs the ADA into change.
        spendSkel =
          spendToaInlined params toaUtxo
          <> spendWalletKey nftUtxo
          <> burn
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S6: reference-input attack
-------------------------------------------------------------------------------

-- | S6 — The /load-bearing/ negative test. W2 owns the controlling NFT (W1
-- never has it). W1 tries to spend a TOA UTxO by including W2's NFT UTxO as
-- a *reference* input. The validator counts only regular spent inputs, so
-- @sumSpentInputs(ac) == 0@ and T1 fires. mustFail.
--
-- This case is the entire reason TOA v1 specifies @txInfoInputs@ (regular)
-- rather than @valueSpent txInfo@ — see the TOA CIP §"Why total quantity,
-- not 'some input contains the NFT'".
s6ReferenceInputAttackFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s6ReferenceInputAttackFails TestInfo {testWallets = Wallets {w1, w2}} = do
  -- W2 holds the controlling NFT; W1 will attempt to drain the TOA without it.
  (nftAC, params) <- mintTestNft w2 "TOA-NFT-S6" 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue Nothing
  -- W1 spends the TOA. The NFT is included only as a REFERENCE input.
  mustFail $ asUser w1 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    nftUtxo <- findOwnedUTxOWithAsset w2 nftAC
    w1Addr  <- ownChangeAddress
    let refOnly = mustHaveRefInput (utxoRef nftUtxo)
        spendSkel =
          spendToaInlined params toaUtxo
          <> refOnly
          <> payNoDatum w1Addr depositValue
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S7: many TOA UTxOs, one NFT input, single carry-through
-------------------------------------------------------------------------------

-- | S7 — Demonstrates that one NFT input authorises spending /any/ number of
-- TOA UTxOs in a single tx (CIP §"Authorization scope"). Five distinct
-- deposits (5/6/7/8/9 ADA, all with the unit datum) are consumed alongside
-- the NFT; outputs: NFT carry-through + balancer-managed change. PASS.
s7ManyToaUtxos :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s7ManyToaUtxos TestInfo {testWallets = Wallets {w1}} = do
  (nftAC, params) <- mintTestNft w1 "TOA-NFT-S7" 1
  -- Distinct values disambiguate the post-submit UTxO lookups.
  refs <-
    mapM
      ( \ada ->
          depositToToaUTxO
            w1
            params
            (valueFromLovelace ada)
            (Just (datumFromPlutusData (), GYTxOutUseInlineDatum @'PlutusV3))
      )
      [5_000_000, 6_000_000, 7_000_000, 8_000_000, 9_000_000]
  asUser w1 $ do
    toaUtxos <- mapM utxoAtTxOutRef' refs
    nftUtxo  <- findOwnedUTxOWithAsset w1 nftAC
    w1Addr   <- ownChangeAddress
    let spendSkel =
          mconcat (spendToaInlined params <$> toaUtxos)
          <> spendWalletKey nftUtxo
          <> payNoDatum w1Addr (valueSingleton nftAC 1)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S8: self-deposit, permissionless spend
-------------------------------------------------------------------------------

-- | S8 — W1 mints the NFT, then deposits the NFT itself at the TOA address.
-- W2 (unrelated) spends that UTxO permissionlessly, carrying the NFT to
-- their own wallet. PASS. (See CIP §"Self-deposit semantics".)
s8SelfDepositPermissionlessSpend :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s8SelfDepositPermissionlessSpend TestInfo {testWallets = Wallets {w1, w2}} = do
  (nftAC, params) <- mintTestNft w1 "TOA-NFT-S8" 1
  let depositValue = valueSingleton nftAC 1 <> valueFromLovelace 5_000_000
  toaRef <-
    depositToToaUTxO
      w1
      params
      depositValue
      (Just (datumFromPlutusData (), GYTxOutUseInlineDatum @'PlutusV3))
  -- W2 (no relation to W1) consumes the self-deposited TOA UTxO.
  asUser w2 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    w2Addr  <- ownChangeAddress
    let spendSkel =
          spendToaInlined params toaUtxo
          <> payNoDatum w2Addr (valueSingleton nftAC 1)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S9: non-conforming deposits remain spendable (3 sub-cases)
-------------------------------------------------------------------------------

-- | S9 — Verify that the validator does NOT reject deposits based on datum
-- shape (CIP §"Datum and redeemer schema"). Parameterised by the deposit
-- datum option: 'Nothing' → no datum, 'Just (d, GYTxOutDontUseInlineDatum)'
-- → hash-only, 'Just (d, GYTxOutUseInlineDatum)' → inline (unit or junk).
-- All three sub-cases PASS.
--
-- The hash-only case constructs the script witness manually to supply the
-- datum preimage. Atlas's pre-submission checks require the preimage even
-- though the TOA validator itself does not inspect the datum.
s9NonConforming ::
  Maybe (GYDatum, GYTxOutUseInlineDatum 'PlutusV3) ->
  TestInfo ->
  GYTxMonadClb ()
s9NonConforming mDatum TestInfo {testWallets = Wallets {w1}} = do
  (nftAC, params) <- mintTestNft w1 "TOA-NFT-S9" 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue mDatum
  asUser w1 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    nftUtxo <- findOwnedUTxOWithAsset w1 nftAC
    w1Addr  <- ownChangeAddress
    let toaInput = case mDatum of
          Just (d, GYTxOutDontUseInlineDatum) ->
            spendToaInlinedWithDatum params toaUtxo d
          _ ->
            spendToaInlined params toaUtxo
        spendSkel =
          toaInput
          <> spendWalletKey nftUtxo
          <> payNoDatum w1Addr (valueSingleton nftAC 1)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S10: wrong asset name (same policy) does not satisfy T1
-------------------------------------------------------------------------------

-- | S10 — Mint two assets under the same policy id but with different asset
-- names. Deposit a TOA UTxO controlled by asset A. Attempt to spend it by
-- consuming only asset B as a regular input. The validator's asset-class
-- equality check (@(policyId, assetName)@) sees @sumSpentInputs(A) == 0@ and
-- T1 fires. mustFail.
--
-- Complements S1/S6 by exercising the asset-class /identity/ check, not just
-- the quantity check.
s10WrongAssetNameFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s10WrongAssetNameFails TestInfo {testWallets = Wallets {w1}} = do
  -- Derive paramsA without minting the control NFT — otherwise Atlas's balancer
  -- coalesces same-policy outputs, producing one wallet UTxO that holds BOTH
  -- assets. The validator would then see sumSpentInputs(ctrl) == 1 and T1
  -- would pass, defeating the test.
  let paramsA = paramsFor "TOA-NFT-S10-CTRL"
  (acB, _paramsB) <- mintTestNft w1 "TOA-NFT-S10-DECOY" 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 paramsA depositValue Nothing
  mustFail $ asUser w1 $ do
    toaUtxo   <- utxoAtTxOutRef' toaRef
    decoyUtxo <- findOwnedUTxOWithAsset w1 acB
    w1Addr    <- ownChangeAddress
    let spendSkel =
          spendToaInlined paramsA toaUtxo
          <> spendWalletKey decoyUtxo
          <> payNoDatum w1Addr (valueSingleton acB 1)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S11: NFT re-deposited at the TOA address (output goes back to TOA)
-------------------------------------------------------------------------------

-- | S11 — Spend a TOA UTxO and place the controlling NFT into a /new/ TOA
-- UTxO at the same address. T2 counts outputs globally (not by address), so
-- the NFT-carry-through requirement is satisfied. PASS.
--
-- Guards against an accidental narrowing of T2 to "non-script outputs",
-- which would break legitimate re-deposit flows.
s11NftRedepositedAtToa :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s11NftRedepositedAtToa TestInfo {testWallets = Wallets {w1}} = do
  (nftAC, params) <- mintTestNft w1 "TOA-NFT-S11" 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue Nothing
  asUser w1 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    nftUtxo <- findOwnedUTxOWithAsset w1 nftAC
    nid     <- networkId
    let toaAddr = toaAddress nid params
        spendSkel =
          spendToaInlined params toaUtxo
          <> spendWalletKey nftUtxo
          <> payInlineDatum toaAddr
               (datumFromPlutusData ())
               (valueSingleton nftAC 1 <> valueFromLovelace 2_000_000)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S12: S6 contrast — NFT owned by W2, consumed as a regular input. PASS.
-------------------------------------------------------------------------------

-- | S12 — Same NFT-ownership setup as S6 (W2 holds the controlling NFT),
-- but W2 itself signs the spend with the NFT as a /regular/ input. PASS.
--
-- Paired with S6 this isolates @reference-input@ vs @spent-input@ as the
-- distinguishing factor: ownership is identical, only the input kind differs.
s12NftRegularInputW2Owner :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s12NftRegularInputW2Owner TestInfo {testWallets = Wallets {w1, w2}} = do
  (nftAC, params) <- mintTestNft w2 "TOA-NFT-S12" 1
  let depositValue = valueFromLovelace 5_000_000
  toaRef <- depositToToaUTxO w1 params depositValue Nothing
  asUser w2 $ do
    toaUtxo <- utxoAtTxOutRef' toaRef
    nftUtxo <- findOwnedUTxOWithAsset w2 nftAC
    w2Addr  <- ownChangeAddress
    let spendSkel =
          spendToaInlined params toaUtxo
          <> spendWalletKey nftUtxo
          <> payNoDatum w2Addr (valueSingleton nftAC 1)
    void $ sendSkeleton' spendSkel

-------------------------------------------------------------------------------
-- S13: many TOA UTxOs + reference-input attack. mustFail T1.
-------------------------------------------------------------------------------

-- | S13 — Combines S6 (reference-input attack) with S7 (many TOA UTxOs). W2
-- holds the NFT; W1 deposits five distinct TOA UTxOs and tries to drain all
-- five in one tx with the NFT only as a reference input. Each validator
-- invocation sees @sumSpentInputs(ac) == 0@; T1 fires. mustFail.
--
-- Confirms the "one NFT authorises N TOA UTxOs" rule does not relax the
-- spent-vs-reference distinction when N > 1.
s13ManyToaReferenceAttackFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
s13ManyToaReferenceAttackFails TestInfo {testWallets = Wallets {w1, w2}} = do
  (nftAC, params) <- mintTestNft w2 "TOA-NFT-S13" 1
  refs <-
    mapM
      ( \ada ->
          depositToToaUTxO
            w1
            params
            (valueFromLovelace ada)
            (Just (datumFromPlutusData (), GYTxOutUseInlineDatum @'PlutusV3))
      )
      [5_000_000, 6_000_000, 7_000_000, 8_000_000, 9_000_000]
  mustFail $ asUser w1 $ do
    toaUtxos <- mapM utxoAtTxOutRef' refs
    nftUtxo  <- findOwnedUTxOWithAsset w2 nftAC
    w1Addr   <- ownChangeAddress
    let refOnly = mustHaveRefInput (utxoRef nftUtxo)
        spendSkel =
          mconcat (spendToaInlined params <$> toaUtxos)
          <> refOnly
          <> payNoDatum w1Addr (valueFromLovelace 35_000_000)
    void $ sendSkeleton' spendSkel
