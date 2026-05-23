{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Application monad for the TOA HTTP API.
--
-- The handler stack is @ReaderT 'AppContext' Servant.Handler@. The single
-- piece of context is a 'ProviderCtx' (from @offchain-lib@) — TOA has no
-- deployed-scripts context because the validator is parametric per-NFT.
--
-- Errors raised from offchain code (Atlas's 'GYTxMonadException' carrying
-- a 'TxBuildingException', or any other thrown 'TxBuildingException') funnel
-- through 'runWithTxErrorHandling', which maps them to a Servant
-- 'ServerError' via 'toServerError'. Handlers stay one-liners.
module Api.AppMonad
  ( AppContext (..)
  , AppMonad
  , runAppMonad
  , runWithTxErrorHandling
  , throwTxBuildingException
  , toServerError
  ) where

import Control.Exception (displayException, try)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, MonadTrans (lift), ReaderT, runReaderT)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Typeable (cast)
import GeniusYield.TxBuilder (GYTxMonadException (..))
import Servant
import TxBuilding.Context
import TxBuilding.Exceptions (TxBuildingException (..), txBuildingExceptionToHttpStatus)
import WebAPI.Auth (AuthContext)

data AppContext = AppContext
  { providerCtx  :: ProviderCtx
  , authContext  :: AuthContext
  }

newtype AppMonad a = AppMonad (ReaderT AppContext Handler a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppContext)

runAppMonad :: AppContext -> AppMonad a -> Handler a
runAppMonad ctx (AppMonad m) = runReaderT m ctx

-- | Throw a 'TxBuildingException' converted to a Servant 'ServerError'.
throwTxBuildingException :: TxBuildingException -> AppMonad a
throwTxBuildingException = AppMonad . lift . throwError . toServerError

-- | Single mapping from project-wide 'TxBuildingException' to Servant
-- response. Body is 'displayException'; status is from
-- 'txBuildingExceptionToHttpStatus'.
toServerError :: TxBuildingException -> ServerError
toServerError e =
  let body   = BL8.pack (displayException e)
      status = txBuildingExceptionToHttpStatus e
   in case status of
        400 -> err400 {errBody = body}
        404 -> err404 {errBody = body}
        422 -> err422 {errBody = body}
        500 -> err500 {errBody = body}
        502 -> err502 {errBody = body}
        503 -> err503 {errBody = body}
        _   -> err500 {errBody = body}

-- | Run an IO action that may throw a 'GYTxMonadException' wrapping a
-- 'TxBuildingException'. The wrapped exception is mapped via 'toServerError';
-- any other 'GYTxMonadException' becomes a 400 with the exception body
-- echoed back.
runWithTxErrorHandling :: IO a -> AppMonad a
runWithTxErrorHandling action = AppMonad $ do
  res <- liftIO $ try action
  case res of
    Right ok -> pure ok
    Left ex ->
      case ex of
        GYApplicationException appE
          | Just txEx <- cast appE -> throwError (toServerError txEx)
        _ -> throwError err400 {errBody = BL8.pack (show (ex :: GYTxMonadException))}
