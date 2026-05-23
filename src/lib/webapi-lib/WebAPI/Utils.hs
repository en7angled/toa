{-# LANGUAGE OverloadedStrings #-}

-- | Shared utilities for the TOA web API: port resolution, config loading,
-- shared Swagger descriptions for Cardano types.
--
-- Adapted from the Decentralized-Belt-System reference implementation
-- (@WebAPI.Utils@ and @Utils.decodeConfigEnvOrFile@).
module WebAPI.Utils
  ( getPortFromEnvOrDefault
  , addSharedSwaggerDescriptions
  , decodeConfigEnvOrFile
  ) where

import Control.Lens (at, mapped, (&), (?~))
import Data.Aeson (FromJSON, eitherDecodeFileStrict, eitherDecodeStrict)
import Data.ByteString.Char8 qualified as BS8
import Data.Swagger (Swagger, definitions, description)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Read the @PORT@ environment variable, falling back to the given default if unset or unparseable.
getPortFromEnvOrDefault :: Int -> IO Int
getPortFromEnvOrDefault defaultPort = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return defaultPort
    Just p -> case readMaybe p of
      Just n -> return n
      Nothing -> do
        putStrLn $ "Warning: invalid PORT value; defaulting to " <> show defaultPort
        return defaultPort

-- | Add descriptions to shared Cardano type definitions in the generated Swagger spec.
addSharedSwaggerDescriptions :: Swagger -> Swagger
addSharedSwaggerDescriptions s =
  s
    & definitions . at "GYAssetClass" . mapped . description
      ?~ "Cardano native asset identifier in format: <56-char policy ID hex>.<asset name hex>"
    & definitions . at "GYAddress" . mapped . description
      ?~ "Bech32-encoded Cardano address (addr_test1... on testnet, addr1... on mainnet)"
    & definitions . at "GYTime" . mapped . description
      ?~ "Timestamp in ISO 8601 format (e.g. 2024-06-15T10:30:00Z)"

-- | Decode a JSON config from an env var (whose value IS the JSON), or fall back to a file path.
decodeConfigEnvOrFile :: (FromJSON a) => String -> FilePath -> IO (Maybe a)
decodeConfigEnvOrFile envName filePath = do
  mVal <- lookupEnv envName
  case mVal of
    Just raw -> do
      putStrLn $ "Parsing config from env var " <> show envName
      case eitherDecodeStrict (BS8.pack raw) of
        Right a -> return (Just a)
        Left err -> error $ "Decoding env var " <> envName <> " failed: " <> err
    Nothing -> do
      putStrLn $ "Parsing config from file " <> filePath
      either (const Nothing) Just <$> eitherDecodeFileStrict filePath
