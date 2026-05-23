-- | Off-chain mirror of 'Onchain.Protocol.Types.TOAParamsV1'.
--
-- Uses Atlas's 'GYMintingPolicyId' / 'GYTokenName' so callers don't have to
-- touch raw Plinth types, and converts on demand via 'toaParamsToOnchain'.
-- JSON encoding is snake_case to match the project convention.
module DomainTypes.Core.Types
  ( TOAParams (..),
    toaParamsToOnchain,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CamelToSnake, CustomJSON (..), FieldLabelModifier, StripPrefix)
import GHC.Generics (Generic)
import GeniusYield.Types
  ( GYMintingPolicyId,
    GYTokenName,
    mintingPolicyIdToCurrencySymbol,
    tokenNameToPlutus,
  )
import Onchain.Protocol.Types qualified as On

data TOAParams = TOAParams
  { toaVersion :: !Integer,
    toaPolicyId :: !GYMintingPolicyId,
    toaAssetName :: !GYTokenName
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "toa", CamelToSnake]] TOAParams

toaParamsToOnchain :: TOAParams -> On.TOAParamsV1
toaParamsToOnchain TOAParams {..} =
  On.TOAParamsV1
    toaVersion
    (mintingPolicyIdToCurrencySymbol toaPolicyId)
    (tokenNameToPlutus toaAssetName)
