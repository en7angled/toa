-- | API-level actions that a TOA HTTP client can request.
--
-- Currently a single constructor ('SpendToaAction'); the sum-type shape is
-- in place for future actions (backend-built deposits, burns, marketplace
-- offers). 'TxBuilding.Interactions.interactionToTxSkeleton' dispatches on
-- this type.
module DomainTypes.Core.Actions
  ( ActionType (..),
  )
where

import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Deriving.Aeson
import GeniusYield.Types (GYAddress, GYMintingPolicyId, GYTokenName, GYValue)
import Utils

data ActionType
  = SpendToaAction
  { atPolicyId :: GYMintingPolicyId,
    atAssetName :: GYTokenName,
    atTargetValue :: GYValue,
    atNftRecipient :: Maybe GYAddress
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "at", CamelToSnake]] ActionType

instance ToSchema ActionType where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "at")
