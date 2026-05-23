{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | TOA-specific HTTP handlers.
--
-- All tx-building goes through 'TxBuilding.Transactions.interactionToHexEncodedCBOR'
-- — the API layer carries no tx-building logic of its own.
module Api.Handlers.Toa
  ( handleDerive,
    handleDeriveBulk,
    handleDeriveTrace,
    handleUtxos,
    handleSpend,

    -- * Pure builder (exposed for tests)
    DeriveTrace (..),
    deriveTrace,
  )
where

import Api.AppMonad
import Api.Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import DomainTypes.Core.Types (TOAParams (..), toaParamsToOnchain)
import GeniusYield.TxBuilder (utxosAtAddresses)
import GeniusYield.Types
import TxBuilding.Context (ProviderCtx, getNetworkId, runQuery)
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Interactions (Interaction)
import TxBuilding.Toa.Address (toaAddress)
import TxBuilding.Toa.Conversions (outDatumCborBytes, outDatumHashBytes, paramsCborBytes)
import TxBuilding.Toa.Fingerprint (cip14Fingerprint)
import TxBuilding.Toa.Validator (toaV1ApplyBytes, toaV1ScriptHash, toaV1UnappliedHashBytes)
import TxBuilding.Transactions (interactionToHexEncodedCBOR)
import Utils (hexBytes, hexText, rawBytes)

-------------------------------------------------------------------------------
-- Derive helpers
-------------------------------------------------------------------------------

paramsOf :: GYMintingPolicyId -> GYTokenName -> TOAParams
paramsOf p a = TOAParams {toaVersion = 1, toaPolicyId = p, toaAssetName = a}

-- | All values derivable from @(network, TOAParams)@ in a single record.
-- The /toa/derive endpoint projects 'dtAddress' and 'dtScriptHash' out of this;
-- /toa/derive/trace returns every field.
data DeriveTrace = DeriveTrace
  { dtAddress :: !GYAddress,
    dtScriptHash :: !GYScriptHash,
    dtTemplateHashHex :: !ByteString,
    dtParamsCborHex :: !ByteString,
    dtAppliedCborHex :: !ByteString,
    dtAppliedBytes :: !Int,
    dtAppliedScriptHashHex :: !ByteString
  }

-- | Derive all TOA trace fields from a network ID and params.
deriveTrace :: GYNetworkId -> TOAParams -> DeriveTrace
deriveTrace nid params =
  let onchain = toaParamsToOnchain params
      sh = toaV1ScriptHash onchain
      appliedRaw = toaV1ApplyBytes onchain
      paramsRaw = paramsCborBytes onchain
   in DeriveTrace
        { dtAddress = toaAddress nid onchain,
          dtScriptHash = sh,
          dtTemplateHashHex = hexBytes toaV1UnappliedHashBytes,
          dtParamsCborHex = hexBytes paramsRaw,
          dtAppliedCborHex = hexBytes appliedRaw,
          dtAppliedBytes = BS.length appliedRaw,
          dtAppliedScriptHashHex = hexBytes (rawBytes (scriptHashToApi sh))
        }

askNetworkId :: AppMonad GYNetworkId
askNetworkId = getNetworkId <$> askProviderCtx

askProviderCtx :: AppMonad ProviderCtx
askProviderCtx = asks providerCtx

-------------------------------------------------------------------------------
-- /toa/derive
-------------------------------------------------------------------------------

handleDerive :: Maybe Text -> Maybe Text -> AppMonad DeriveResponse
handleDerive mPolicy mAsset = do
  nid <- askNetworkId
  policyText <- requireParam "policy_id" mPolicy
  assetText <- requireParam "asset_name" mAsset
  policy <- parsePolicyId policyText
  asset <- parseAssetName assetText
  let dt = deriveTrace nid (paramsOf policy asset)
  pure DeriveResponse {drAddress = dtAddress dt, drScriptHash = dtScriptHash dt}

-------------------------------------------------------------------------------
-- /toa/derive/bulk
-------------------------------------------------------------------------------

handleDeriveBulk :: [BulkDeriveItem] -> AppMonad [BulkDeriveResponseItem]
handleDeriveBulk items = do
  nid <- askNetworkId
  pure
    [ let dt = deriveTrace nid (paramsOf bdiPolicyId bdiAssetName)
       in BulkDeriveResponseItem
            { bdrPolicyId = bdiPolicyId,
              bdrAssetName = bdiAssetName,
              bdrAddress = dtAddress dt,
              bdrScriptHash = dtScriptHash dt
            }
      | BulkDeriveItem {..} <- items
    ]

-------------------------------------------------------------------------------
-- /toa/derive/trace
-------------------------------------------------------------------------------

-- | Return the full derivation trace. Adds @toa_version@ as an optional query
-- param (default 1) so the 6th committed vector (ascii-v2, version 2) is
-- reachable; v1 callers don't need to pass it.
handleDeriveTrace ::
  Maybe Text ->
  Maybe Text ->
  Maybe Integer ->
  AppMonad DeriveTraceResponse
handleDeriveTrace mPolicy mAsset mVersion = do
  nid <- askNetworkId
  policyText <- requireParam "policy_id" mPolicy
  assetText <- requireParam "asset_name" mAsset
  policy <- parsePolicyId policyText
  asset <- parseAssetName assetText
  let version = case mVersion of
        Nothing -> 1
        Just v -> v
      params = TOAParams {toaVersion = version, toaPolicyId = policy, toaAssetName = asset}
      dt = deriveTrace nid params
      fp = cip14Fingerprint policy asset
  pure
    DeriveTraceResponse
      { dtrPolicyId = policy,
        dtrAssetNameHex = asset,
        dtrCip14Fingerprint = fp,
        dtrToaVersion = version,
        dtrTemplateHash = TE.decodeUtf8 (dtTemplateHashHex dt),
        dtrParamsCborHex = TE.decodeUtf8 (dtParamsCborHex dt),
        dtrAppliedScriptCborHex = TE.decodeUtf8 (dtAppliedCborHex dt),
        dtrAppliedScriptBytes = dtAppliedBytes dt,
        dtrAppliedScriptHash = TE.decodeUtf8 (dtAppliedScriptHashHex dt),
        dtrAddress = dtAddress dt
      }

-------------------------------------------------------------------------------
-- /toa/utxos
-------------------------------------------------------------------------------

handleUtxos :: Maybe Text -> AppMonad UtxosResponse
handleUtxos mAddr = do
  ctx <- askProviderCtx
  addrText <- requireParam "address" mAddr
  addr <- parseAddress addrText
  utxos <- liftIO $ runQuery ctx (utxosToList <$> utxosAtAddresses [addr])
  let balance = foldMap utxoValue utxos
      summaries = map summarise utxos
  pure UtxosResponse {urAddress = addr, urUtxos = summaries, urBalance = balance}
  where
    summarise u =
      let od = utxoOutDatum u
       in UtxoSummary
            { usRef = utxoRef u,
              usValue = utxoValue u,
              usDatumCborHex = hexText <$> outDatumCborBytes od,
              usDatumHash = hexText <$> outDatumHashBytes od
            }

-------------------------------------------------------------------------------
-- /toa/spend
-------------------------------------------------------------------------------

handleSpend :: Interaction -> AppMonad TxCborResponse
handleSpend interaction = do
  ctx <- askProviderCtx
  hex <- runWithTxErrorHandling (interactionToHexEncodedCBOR ctx interaction)
  pure TxCborResponse {tcrTxCborHex = T.pack hex}

-------------------------------------------------------------------------------
-- Parsing helpers
-------------------------------------------------------------------------------

requireParam :: Text -> Maybe Text -> AppMonad Text
requireParam name = maybe (throwTxBuildingException (InvalidParams ("Missing query parameter: " <> name))) pure

parsePolicyId :: Text -> AppMonad GYMintingPolicyId
parsePolicyId t = case mintingPolicyIdFromText t of
  Right p -> pure p
  Left err -> throwTxBuildingException (InvalidParams ("Invalid policy_id: " <> T.pack err))

parseAssetName :: Text -> AppMonad GYTokenName
parseAssetName t = case tokenNameFromHex t of
  Right a -> pure a
  Left err -> throwTxBuildingException (InvalidParams ("Invalid asset_name hex: " <> err))

parseAddress :: Text -> AppMonad GYAddress
parseAddress t = case addressFromTextMaybe t of
  Just a -> pure a
  Nothing -> throwTxBuildingException (InvalidParams ("Invalid bech32 address: " <> t))
