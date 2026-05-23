{-# LANGUAGE OverloadedStrings #-}

-- | TOA v1 validator tests.
--
-- This iteration ships pure unit tests that exercise the compiled validator
-- artifact at the byte level. The full CLB scenario suite (positive baseline,
-- two-NFT-input/output failures, mint/burn failures, reference-input attack,
-- multi-TOA-UTxO carry-through, self-deposit, non-conforming-datum spend) is
-- /scaffolded but not implemented/ — see TODO below.
--
-- The pure tests below verify the load-bearing /shape/ invariants the rest
-- of the standard depends on (28-byte template hash, applied-script-hash
-- differs per @(toaVersion, policyId, assetName)@). Address-derivation
-- regression coverage flows through @cabal run toa-gen-vectors@ + the
-- checked-in @test-vectors/toa-v1.json@.
module UnitTests.Validator
  ( validatorTests
  ) where

import Data.ByteString qualified as BS
import Onchain.Protocol.Types (TOAParamsV1 (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import GeniusYield.Types (GYScriptHash, scriptHashToApi)
import TxBuilding.Toa.Validator (toaV1ScriptHash, toaV1UnappliedHashBytes)
import UnitTests.Validator.Scenarios qualified as Scenarios
import Utils (rawBytes)

validatorTests :: TestTree
validatorTests =
  testGroup
    "Validator Tests"
    [ testGroup
        "Pure unit tests"
        [ testCase "template hash is 28 bytes (CIP §Mandatory committed artifacts)" templateHashSize
        , testCase "applied script hash is 28 bytes" appliedHashSize
        , testCase "toaVersion changes the applied script hash" toaVersionAffectsHash
        , testCase "policyId changes the applied script hash" policyIdAffectsHash
        , testCase "assetName changes the applied script hash" assetNameAffectsHash
        ]
    , Scenarios.scenarioTests
    ]

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

samplePolicy :: CurrencySymbol
samplePolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x01))

altPolicy :: CurrencySymbol
altPolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x02))

sampleName :: TokenName
sampleName = TokenName (toBuiltin (BS.pack [0xAA, 0xBB, 0xCC]))

altName :: TokenName
altName = TokenName (toBuiltin (BS.pack [0xDD, 0xEE]))

paramsBase :: TOAParamsV1
paramsBase = TOAParamsV1 1 samplePolicy sampleName

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

templateHashSize :: Assertion
templateHashSize = BS.length toaV1UnappliedHashBytes @?= 28

appliedHashSize :: Assertion
appliedHashSize =
  BS.length (rawHash (toaV1ScriptHash paramsBase)) @?= 28

toaVersionAffectsHash :: Assertion
toaVersionAffectsHash = do
  let h1 = toaV1ScriptHash paramsBase
      h2 = toaV1ScriptHash paramsBase {toaVersion = 2}
  assertBool "toaVersion=1 vs 2 must yield different applied hashes" (h1 /= h2)

policyIdAffectsHash :: Assertion
policyIdAffectsHash = do
  let h1 = toaV1ScriptHash paramsBase
      h2 = toaV1ScriptHash paramsBase {toaPolicyId = altPolicy}
  assertBool "different policyId must yield different applied hashes" (h1 /= h2)

assetNameAffectsHash :: Assertion
assetNameAffectsHash = do
  let h1 = toaV1ScriptHash paramsBase
      h2 = toaV1ScriptHash paramsBase {toaAssetName = altName}
  assertBool "different assetName must yield different applied hashes" (h1 /= h2)

rawHash :: GYScriptHash -> BS.ByteString
rawHash = rawBytes . scriptHashToApi
