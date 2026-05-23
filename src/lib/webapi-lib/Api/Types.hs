{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
-- | JSON request/response types for the TOA HTTP API.
module Api.Types
  ( -- * Derive
    DeriveResponse (..),
    DeriveTraceResponse (..),
    BulkDeriveItem (..),
    BulkDeriveResponseItem (..),

    -- * UTxO query
    UtxoSummary (..),
    UtxosResponse (..),

    -- * Spend (request body is 'TxBuilding.Interactions.Interaction')
    TxCborResponse (..),

    -- * Submit
    SubmitRequest (..),
    SubmitResponse (..),

    -- * Tx status
    TxStatusResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Text (Text)
import Deriving.Aeson (CamelToSnake, CustomJSON (..), FieldLabelModifier, StripPrefix)
import GHC.Generics (Generic)
import GeniusYield.Types
  ( GYAddress,
    GYMintingPolicyId,
    GYScriptHash,
    GYTokenName,
    GYTxId,
    GYTxOutRef,
    GYValue,
  )
import Utils (mkStripPrefixSchemaOptions)
import WebAPI.AtlasOrphans ()

-------------------------------------------------------------------------------
-- Derive
-------------------------------------------------------------------------------

data DeriveResponse = DeriveResponse
  { drAddress :: GYAddress,
    drScriptHash :: GYScriptHash
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "dr", CamelToSnake]] DeriveResponse

instance ToSchema DeriveResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "dr")

data BulkDeriveItem = BulkDeriveItem
  { bdiPolicyId :: GYMintingPolicyId,
    bdiAssetName :: GYTokenName
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "bdi", CamelToSnake]] BulkDeriveItem

instance ToSchema BulkDeriveItem where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "bdi")

data BulkDeriveResponseItem = BulkDeriveResponseItem
  { bdrPolicyId :: GYMintingPolicyId,
    bdrAssetName :: GYTokenName,
    bdrAddress :: GYAddress,
    bdrScriptHash :: GYScriptHash
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "bdr", CamelToSnake]] BulkDeriveResponseItem

instance ToSchema BulkDeriveResponseItem where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "bdr")

data DeriveTraceResponse = DeriveTraceResponse
  { dtrPolicyId :: GYMintingPolicyId,
    dtrAssetNameHex :: GYTokenName,
    dtrCip14Fingerprint :: Text,
    dtrToaVersion :: Integer,
    dtrTemplateHash :: Text,
    dtrParamsCborHex :: Text,
    dtrAppliedScriptCborHex :: Text,
    dtrAppliedScriptBytes :: Int,
    dtrAppliedScriptHash :: Text,
    dtrAddress :: GYAddress
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "dtr", CamelToSnake]] DeriveTraceResponse

instance ToSchema DeriveTraceResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "dtr")

-------------------------------------------------------------------------------
-- UTxOs at a TOA address
-------------------------------------------------------------------------------

-- | JSON-friendly projection of 'GYUTxO' (Atlas doesn't expose JSON for the
-- full record). Captures the fields a frontend needs to render a TOA balance.
--
-- @datum_cbor_hex@ is the raw CBOR (lower-case hex) of an inline datum;
-- @null@ for hash-only and absent outputs. @datum_hash@ is the blake2b_256
-- of the datum bytes (lower-case hex), populated for both inline and
-- hash-only outputs; @null@ only when the output has no datum at all.
data UtxoSummary = UtxoSummary
  { usRef :: GYTxOutRef,
    usValue :: GYValue,
    usDatumCborHex :: Maybe Text,
    usDatumHash :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "us", CamelToSnake]] UtxoSummary

instance ToSchema UtxoSummary where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "us")

data UtxosResponse = UtxosResponse
  { urAddress :: GYAddress,
    urUtxos :: [UtxoSummary],
    urBalance :: GYValue
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ur", CamelToSnake]] UtxosResponse

instance ToSchema UtxosResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "ur")

-------------------------------------------------------------------------------
-- Spend response (request body is 'TxBuilding.Interactions.Interaction')
-------------------------------------------------------------------------------

newtype TxCborResponse = TxCborResponse
  { tcrTxCborHex :: Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "tcr", CamelToSnake]] TxCborResponse

instance ToSchema TxCborResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "tcr")

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

newtype SubmitRequest = SubmitRequest
  { sbrTxCborHex :: Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "sbr", CamelToSnake]] SubmitRequest

instance ToSchema SubmitRequest where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "sbr")

newtype SubmitResponse = SubmitResponse
  { srTxId :: GYTxId
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "sr", CamelToSnake]] SubmitResponse

instance ToSchema SubmitResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "sr")

-------------------------------------------------------------------------------
-- Tx status (confirmation polling)
-------------------------------------------------------------------------------

data TxStatusResponse = TxStatusResponse
  { tsrTxId :: GYTxId,
    tsrConfirmed :: Bool
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "tsr", CamelToSnake]] TxStatusResponse

instance ToSchema TxStatusResponse where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "tsr")

