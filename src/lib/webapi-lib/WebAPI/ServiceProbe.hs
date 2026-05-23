{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generic Servant health/readiness probe endpoints.
--
-- Lifted from the Decentralized-Belt-System reference implementation.
module WebAPI.ServiceProbe where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Servant

data ServiceProbeStatus a = ServiceProbeStatus
  { status :: a
  , service :: Text
  , version :: Text
  , timestamp :: Text
  }
  deriving (Generic, Show)

instance (ToJSON a) => ToJSON (ServiceProbeStatus a)

instance (FromJSON a) => FromJSON (ServiceProbeStatus a)

instance (ToSchema a) => ToSchema (ServiceProbeStatus a)

type ServiceProbe h r =
  ( Summary "Health Check"
      :> Description "Returns the health status of the service"
      :> "health"
      :> Get '[JSON] (ServiceProbeStatus h)
  )
    :<|> ( Summary "Readiness Check"
            :> Description "Returns the readiness status of the service"
            :> "ready"
            :> Get '[JSON] (ServiceProbeStatus r)
         )

mkProbeStatus :: (MonadIO m) => Text -> Text -> Text -> m (ServiceProbeStatus Text)
mkProbeStatus statusText versionText serviceName = do
  now <- liftIO getCurrentTime
  return $
    ServiceProbeStatus
      { status = statusText
      , service = serviceName
      , version = versionText
      , timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      }

alwaysHealthy :: (MonadIO m) => Text -> Text -> m (ServiceProbeStatus Text)
alwaysHealthy = mkProbeStatus "healthy"

alwaysReady :: (MonadIO m) => Text -> Text -> m (ServiceProbeStatus Text)
alwaysReady = mkProbeStatus "ready"
