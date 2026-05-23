{-# LANGUAGE LambdaCase #-}

module TxBuilding.Exceptions
  ( TxBuildingException (..)
  , txBuildingExceptionToHttpStatus
  ) where

import Control.Exception (Exception (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.Types (GYValue)

data TxBuildingException
  = -- | The controlling NFT was not found at any of the supplied wallet addresses.
    NFTNotFound
  | -- | The TOA address holds less value than the requested withdrawal.
    InsufficientToaValue {requested :: GYValue, available :: GYValue}
  | -- | Multiple UTxOs were found for the specified asset class.
    MultipleUtxosFound
  | -- | The specified asset class is invalid.
    InvalidAssetClass
  | -- | A request parameter was missing or syntactically invalid.
    InvalidParams Text
  | -- | A chain provider call failed (Maestro / Blockfrost / etc.).
    ProviderError Text
  deriving stock (Generic, Show, Eq)

instance Exception TxBuildingException where
  displayException :: TxBuildingException -> String
  displayException = \case
    NFTNotFound ->
      "Controlling NFT not found in supplied wallet addresses"
    InsufficientToaValue req av ->
      "Insufficient value at TOA address. Requested " <> show req <> ", available " <> show av
    MultipleUtxosFound ->
      "Multiple UTxOs found for this asset class"
    InvalidAssetClass ->
      "Invalid asset class specified"
    InvalidParams msg ->
      T.unpack msg
    ProviderError msg ->
      "Provider error: " <> T.unpack msg

instance IsGYApiError TxBuildingException

txBuildingExceptionToHttpStatus :: TxBuildingException -> Int
txBuildingExceptionToHttpStatus = \case
  NFTNotFound -> 404
  InsufficientToaValue {} -> 422
  MultipleUtxosFound -> 400
  InvalidAssetClass -> 400
  InvalidParams {} -> 400
  ProviderError {} -> 502
