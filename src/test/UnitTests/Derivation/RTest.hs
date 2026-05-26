{-# LANGUAGE OverloadedStrings #-}

-- | Cross-check that the offchain R reflection ('TxBuilding.Toa.DerivationR')
-- reproduces every published category-(a) address-derivation test vector
-- in @test-vectors/toa-v1.json@ byte-for-byte. The vendored JSON is the
-- source of truth.
module UnitTests.Derivation.RTest
  ( tests
  ) where

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as BSL
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertEqual, testCase)

import qualified TxBuilding.Toa.DerivationR as R

vectorPath :: FilePath
vectorPath = "test-vectors/toa-v1.json"

tests :: TestTree
tests =
  testGroup
    "Onchain.Derivation.R (offchain reflection)"
    [ testCase "R reproduces every published expected_script_hash byte-for-byte"
        checkAllVectors
    ]

checkAllVectors :: IO ()
checkAllVectors = do
  raw <- BSL.readFile vectorPath
  case Aeson.eitherDecode raw of
    Left  e -> error ("toa-v1.json parse failed: " ++ e)
    Right v ->
      case Aeson.parseEither envelopeParser v of
        Left  e  -> error ("envelope parse failed: " ++ e)
        Right vs -> mapM_ checkOne vs
  where
    envelopeParser :: Aeson.Value -> Aeson.Parser [Aeson.Value]
    envelopeParser = Aeson.withObject "envelope" $ \o -> o Aeson..: "vectors"

checkOne :: Aeson.Value -> IO ()
checkOne v = case Aeson.parseEither parseVec v of
  Left  e -> error ("vector parse failed: " ++ e)
  Right (name, toaVersion, pidHex, anHex, expected) -> do
    let pid    = decodeHexOrDie ("policy_id of " <> Text.unpack name)      pidHex
        an     = decodeHexOrDie ("asset_name_hex of " <> Text.unpack name) anHex
        got    = R.toaScriptHash toaVersion pid an
        gotHex = Text.decodeUtf8 (B16.encode got)
    assertEqual ("vector " ++ Text.unpack name) expected gotHex

parseVec :: Aeson.Value -> Aeson.Parser (Text, Integer, Text, Text, Text)
parseVec = Aeson.withObject "vector" $ \o -> (,,,,)
  <$> o Aeson..: "name"
  <*> o Aeson..: "toa_version"
  <*> o Aeson..: "policy_id"
  <*> o Aeson..: "asset_name_hex"
  <*> o Aeson..: "expected_script_hash"

decodeHexOrDie :: String -> Text -> ByteString
decodeHexOrDie ctx t =
  case B16.decode (Text.encodeUtf8 t) of
    Right b -> b
    Left  e -> error ("decodeHex (" <> ctx <> "): " <> e)
