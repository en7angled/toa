-- | TOA HTTP API server entry point.
--
-- Loads Atlas Maestro config from @ATLAS_CORE_CONFIG@ env var (JSON value) or
-- @config/config_atlas.json@, starts @withCfgProviders@, and runs warp on
-- @PORT@ (default 8080). Use @cabal run toa-gen-swagger@ to regenerate the
-- committed Swagger JSON when the API shape changes.
module Main (main) where

import Api.AppMonad (AppContext (..))
import Api.Server (mkApp)
import Data.String (IsString (..))
import Data.Text qualified as T
import GeniusYield.GYConfig (cfgNetworkId, withCfgProviders)
import GeniusYield.Types (GYLogNamespace)
import Network.Wai.Handler.Warp
import System.Exit (die)
import TxBuilding.Context (ProviderCtx (..))
import WebAPI.Auth (AuthContext (..), getBasicAuthFromEnv)
import WebAPI.Utils (decodeConfigEnvOrFile, getPortFromEnvOrDefault)

main :: IO ()
main = do
  atlasConfig <-
    maybe (die "Atlas configuration failed (set ATLAS_CORE_CONFIG or provide config/config_atlas.json)") pure
      =<< decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" "config/config_atlas.json"

  auth <- getBasicAuthFromEnv

  withCfgProviders atlasConfig (read @GYLogNamespace "TOA") $ \providers -> do
    let provCtx = ProviderCtx {ctxCoreCfg = atlasConfig, ctxProviders = providers}
        appContext = AppContext {providerCtx = provCtx, authContext = auth}

    let host = "0.0.0.0"
    port <- getPortFromEnvOrDefault 8080
    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings

    putStrLn $ "TOA API listening at http://" <> host <> ":" <> show port
    putStrLn $ "Swagger UI:    http://" <> host <> ":" <> show port <> "/swagger-ui"
    putStrLn $ "Network ID:    " <> show (cfgNetworkId atlasConfig)
    putStrLn $ "Basic auth required for /toa/* and /tx/* (user: " <> T.unpack (authUser auth) <> ")"

    runSettings settings (mkApp appContext)
