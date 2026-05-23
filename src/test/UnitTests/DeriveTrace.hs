{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Cross-checks 'deriveTrace' against the 6 committed CIP test vectors
-- in @test-vectors/toa-v1.json@. Every output field — template hash, params
-- CBOR, applied script CBOR + length + hash, fingerprint — must match the
-- vendored vector byte-for-byte. The vendored JSON is the source of truth;
-- if 'deriveTrace' ever produces a different value, this test fails.
module UnitTests.DeriveTrace
  ( deriveTraceTests
  ) where

import Api.Handlers.Toa (DeriveTrace (..), deriveTrace)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Deriving.Aeson (CamelToSnake, CustomJSON (..), FieldLabelModifier, StripPrefix)
import DomainTypes.Core.Types (TOAParams (..))
import GHC.Generics (Generic)
import GeniusYield.Types
  ( GYNetworkId (..)
  , mintingPolicyIdFromText
  , tokenNameFromHex
  )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import TxBuilding.Toa.Fingerprint (cip14Fingerprint)

-------------------------------------------------------------------------------
-- Vendored JSON shape
-------------------------------------------------------------------------------

data Envelope = Envelope
  { eVectors :: [Vector]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "e", CamelToSnake]] Envelope

data Vector = Vector
  { vName                 :: Text
  , vToaVersion           :: Integer
  , vPolicyId             :: Text
  , vAssetNameHex         :: Text
  , vCip14Fingerprint     :: Text
  , vParamsCborHex        :: Text
  , vAppliedScriptCborHex :: Text
  , vAppliedScriptBytes   :: Int
  , vExpectedScriptHash   :: Text
  , vUnappliedScriptHash  :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "v", CamelToSnake]] Vector

-------------------------------------------------------------------------------
-- Test tree
-------------------------------------------------------------------------------

vectorPath :: FilePath
vectorPath = "test-vectors/toa-v1.json"

deriveTraceTests :: TestTree
deriveTraceTests =
  withResource loadVectors (const (pure ())) $ \getVecs ->
    testGroup
      "deriveTrace cross-check (test-vectors/toa-v1.json)"
      [ testCase "all 6 vectors match byte-for-byte" $ do
          vecs <- getVecs
          mapM_ assertVector vecs
      ]

loadVectors :: IO [Vector]
loadVectors = do
  res <- eitherDecodeFileStrict @Envelope vectorPath
  case res of
    Right (Envelope vs) -> pure vs
    Left err -> assertFailure ("failed to parse " <> vectorPath <> ": " <> err) >> pure []

assertVector :: Vector -> IO ()
assertVector Vector {..} = do
  let scope :: String -> String
      scope msg = Text.unpack vName <> ": " <> msg
  policy <- either (fail . scope . ("bad policy_id: " <>)) pure (mintingPolicyIdFromText vPolicyId)
  asset  <- either (fail . scope . ("bad asset_name: " <>) . Text.unpack) pure (tokenNameFromHex vAssetNameHex)
  let params = TOAParams { toaVersion = vToaVersion, toaPolicyId = policy, toaAssetName = asset }
      dt = deriveTrace GYTestnetPreprod params
  assertEqual (scope "template_hash") (asHex vUnappliedScriptHash) (dtTemplateHashHex dt)
  assertEqual (scope "params_cbor_hex") (asHex vParamsCborHex) (dtParamsCborHex dt)
  assertEqual (scope "applied_script_cbor_hex") (asHex vAppliedScriptCborHex) (dtAppliedCborHex dt)
  assertEqual (scope "applied_script_bytes") vAppliedScriptBytes (dtAppliedBytes dt)
  assertEqual (scope "applied_script_hash") (asHex vExpectedScriptHash) (dtAppliedScriptHashHex dt)
  assertEqual (scope "cip14_fingerprint") vCip14Fingerprint (cip14Fingerprint policy asset)

-- | Convert a Text hex literal to the lower-case ByteString form 'Base16.encode' produces.
asHex :: Text -> ByteString
asHex = TE.encodeUtf8 . Text.toLower
