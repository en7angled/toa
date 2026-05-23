{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for Atlas types that the TOA HTTP API surface needs.
--
-- Atlas does not ship these; hosting them here (rather than in 'Api.Types')
-- keeps the JSON-types module focused on the wire contract and free of the
-- @-Wno-orphans@ pragma.
--
-- Re-export by importing this module wherever the instances are required;
-- 'Api.Types' does so directly so the schema-deriving instances on the
-- response types can see 'ToSchema GYScriptHash'.
module WebAPI.AtlasOrphans () where

import Control.Lens
import Data.Swagger (NamedSchema (..), SwaggerType (..), ToSchema (..))
import Data.Swagger.Internal.ParamSchema (ToParamSchema (..))
import Data.Swagger.Lens
import Data.Text qualified as T
import GeniusYield.Types (GYScriptHash, GYTxId, txIdFromHex)
import Servant (FromHttpApiData (..))

instance ToSchema GYScriptHash where
  declareNamedSchema _ = pure $ NamedSchema (Just "GYScriptHash") mempty

-- | Parse a GYTxId from a URL capture. Atlas does not ship this instance.
instance FromHttpApiData GYTxId where
  parseUrlPiece t = case txIdFromHex (T.unpack t) of
    Just txId -> Right txId
    Nothing -> Left ("Invalid transaction ID: " <> t)

instance ToParamSchema GYTxId where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
