{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Interaction dispatch: an 'Interaction' bundles an 'ActionType' with the
-- user's wallet addresses (plus an optional value recipient). Handlers send
-- one 'Interaction' to 'interactionToTxSkeleton', which dispatches to the
-- right TOA tx-skeleton builder.
--
-- Mirrors DBS @TxBuilding.Interactions@ (per
-- @.cursor/rules/architecture/offchain-rules.mdc@ §Pipeline), simplified for
-- TOA: no @DeployedScriptsContext@ reader, no @Maybe GYAssetClass@ return
-- channel.
module TxBuilding.Interactions
  ( UserAddresses (..),
    Interaction (..),
    AddWitAndSubmitParams (..),
    interactionToTxSkeleton,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Deriving.Aeson (CamelToSnake, CustomJSON (..), FieldLabelModifier, StripPrefix)
import DomainTypes.Core.Actions (ActionType (..))
import GHC.Generics (Generic)
import GeniusYield.TxBuilder (GYTxSkeleton, GYTxUserQueryMonad)
import GeniusYield.Types
  ( GYAddress,
    GYAssetClass (GYToken),
    GYNetworkId,
    GYTx,
    GYTxOutRefCbor,
    GYTxWitness,
    PlutusVersion (PlutusV3),
    tokenNameToPlutus,
    valueSingleton,
  )
import GeniusYield.Types.Script (mintingPolicyIdToCurrencySymbol)
import Onchain.Protocol.Types
import TxBuilding.Toa.Address (toaAddress)
import TxBuilding.Toa.Query
import TxBuilding.Toa.Skeletons
import TxBuilding.Toa.Validator (toaV1Script)
import Utils (mkStripPrefixSchemaOptions)

-------------------------------------------------------------------------------
-- Wallet bundle
-------------------------------------------------------------------------------

-- | The wallet inputs Atlas needs to balance any transaction: used addresses,
-- the change address, and an optional reserved collateral UTxO ref.
data UserAddresses = UserAddresses
  { usedAddresses :: ![GYAddress],
    changeAddress :: !GYAddress,
    reservedCollateral :: !(Maybe GYTxOutRefCbor)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-------------------------------------------------------------------------------
-- Interaction
-------------------------------------------------------------------------------

-- | One API-level interaction: an action + the wallet inputs to satisfy it,
-- plus an optional recipient for value unlocked by the action. 'Nothing'
-- recipient defaults to the user's change address.
data Interaction = Interaction
  { action :: !ActionType,
    userAddresses :: !UserAddresses,
    recipient :: !(Maybe GYAddress)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-------------------------------------------------------------------------------
-- Witness-set submission payload (DBS parity, useful for some wallets)
-------------------------------------------------------------------------------

-- | Witness-set submission payload used by clients that send the signed-witness
-- bytes separately from the unsigned body. The handler combines them via
-- @makeSignedTransaction@ before submission. TOA's default @/tx/submit@ takes
-- the fully-signed CBOR instead; this type is offered for parity with DBS.
data AddWitAndSubmitParams = AddWitAndSubmitParams
  { awasTxUnsigned :: !GYTx,
    awasTxWit :: !GYTxWitness
  }
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "awas", CamelToSnake]] AddWitAndSubmitParams

instance ToSchema AddWitAndSubmitParams where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "awas")

-------------------------------------------------------------------------------
-- Dispatch
-------------------------------------------------------------------------------

-- | Dispatch an 'Interaction' to the matching tx-skeleton builder.
--
-- For TOA v1 the table has a single entry — 'SpendToaAction'. Add a new case
-- here whenever a new constructor is added to 'ActionType'.
interactionToTxSkeleton ::
  (GYTxUserQueryMonad m) =>
  GYNetworkId ->
  Interaction ->
  m (GYTxSkeleton 'PlutusV3)
interactionToTxSkeleton nid Interaction {action, userAddresses = UserAddresses {..}, recipient} = do
  let valueRecipient = fromMaybe changeAddress recipient
  case action of
    SpendToaAction
      policy_id
      asset_name
      target_value
      nft_recipient ->
        do
          let ac = GYToken policy_id asset_name
              plutusPolicyId = mintingPolicyIdToCurrencySymbol policy_id
              plutusAssetName = tokenNameToPlutus asset_name
              params = TOAParamsV1 toaVersionV1 plutusPolicyId plutusAssetName
              validator = toaV1Script params
              toaAddress' = toaAddress nid params
              nftDestination = fromMaybe changeAddress nft_recipient
          (utxosWithDatums, excessValue) <- getUTxOsAtAddressCoveringValue toaAddress' target_value
          nftIsSpent <- txMustSpendNFT ac
          let isSpendingUtxosFromScript = txMustSpendUTXOsFromScript utxosWithDatums validator
              isPayingValue = txMustPayValueToAddress valueRecipient target_value
              isPayingExcessValueBack = txMustPayValueToAddressWithDatum toaAddress' excessValue
              nftIsPayed = txMustPayValueToAddress nftDestination (valueSingleton ac 1)
              nftCarriedThrough = nftIsPayed <> nftIsSpent
          return $
            mconcat
              [ isSpendingUtxosFromScript,
                isPayingExcessValueBack,
                isPayingValue,
                nftCarriedThrough
              ]
