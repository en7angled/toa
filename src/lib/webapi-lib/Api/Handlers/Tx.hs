{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Transaction submission handlers.
module Api.Handlers.Tx
  ( handleSubmit,
    handleSign,
    handleTxStatus,
  )
where

import Api.AppMonad
import Api.Types
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Either (isRight)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GeniusYield.Types
  ( GYAwaitTxParameters (..),
    GYTxId,
    getTxBody,
    gyAwaitTxConfirmed,
    makeSignedTransaction,
    txFromHexBS,
    txToHex,
  )
import TxBuilding.Context (ctxProviders, submitTx)
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Interactions (AddWitAndSubmitParams (..))

handleSubmit :: SubmitRequest -> AppMonad SubmitResponse
handleSubmit SubmitRequest {..} = do
  ctx <- asks providerCtx
  tx <- case txFromHexBS (T.encodeUtf8 sbrTxCborHex) of
    Right t -> pure t
    Left err -> throwTxBuildingException (InvalidParams ("Invalid signed tx CBOR hex: " <> T.pack err))
  txId <- runWithTxErrorHandling (submitTx ctx tx)
  pure SubmitResponse {srTxId = txId}

-- | Combine an unsigned tx with a wallet-produced witness set server-side
-- and return the resulting signed CBOR. The client submits via its wallet
-- (CIP-30 @submitTx@), keeping the backend off the chain-submission path.
-- Combine pattern mirrors DBS @handleSubmitTx@: extract the body, then
-- attach only the wallet's key witnesses.
handleSign :: AddWitAndSubmitParams -> AppMonad TxCborResponse
handleSign AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
      signedTx = makeSignedTransaction awasTxWit txBody
  pure TxCborResponse {tcrTxCborHex = T.pack (txToHex signedTx)}

-- | One-shot confirmation check. Returns within ~100ms whether the tx is
-- confirmed; the client polls this on its own schedule. Mirrors DBS
-- `pollTxConfirmation`.
handleTxStatus :: GYTxId -> AppMonad TxStatusResponse
handleTxStatus txId = do
  providers <- asks (ctxProviders . providerCtx)
  -- Single attempt, 100ms tolerance, 1 confirmation needed.
  let params = GYAwaitTxParameters 1 100000 1
  res <- liftIO $ try @SomeException $ gyAwaitTxConfirmed providers params txId
  pure
    TxStatusResponse
      { tsrTxId = txId,
        tsrConfirmed = isRight res
      }
