-- | Shared helpers for the off-chain libraries.
module Utils
  ( mkStripPrefixSchemaOptions
  , rawBytes
  , hexBytes
  , hexText
  , currencySymbolBytes
  , tokenNameBytes
  ) where

import Cardano.Api qualified as Api
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Swagger.SchemaOptions (SchemaOptions, fromAesonOptions)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins qualified as PTxB

-- | Serialise any Atlas type whose Api representation has a SerialiseAsRawBytes
-- instance. Use as @rawBytes (scriptHashToApi sh)@ rather than the qualified
-- 'Api.serialiseToRawBytes' call directly — communicates intent more clearly.
rawBytes :: Api.SerialiseAsRawBytes a => a -> ByteString
rawBytes = Api.serialiseToRawBytes

-- | Base-16 encode raw bytes to an ASCII hex 'ByteString' (lower-case).
hexBytes :: ByteString -> ByteString
hexBytes = Base16.encode

-- | Base-16 encode raw bytes to a lower-case hex 'Text'.
hexText :: ByteString -> Text
hexText = TE.decodeUtf8 . Base16.encode

-- | Extract the raw 'ByteString' from a Plinth 'CurrencySymbol'.
currencySymbolBytes :: CurrencySymbol -> ByteString
currencySymbolBytes (CurrencySymbol bs) = PTxB.fromBuiltin bs

-- | Extract the raw 'ByteString' from a Plinth 'TokenName'.
tokenNameBytes :: TokenName -> ByteString
tokenNameBytes (TokenName bs) = PTxB.fromBuiltin bs

-- | Build 'SchemaOptions' matching @deriving-aeson@'s
-- @'[StripPrefix' p, 'CamelToSnake']@. Use this in 'ToSchema' instances so
-- the Swagger schema field names line up with the Aeson JSON encoding:
--
-- @
-- data Foo = Foo { fooBar :: Int }
--   deriving (FromJSON, ToJSON)
--     via CustomJSON '[FieldLabelModifier '[StripPrefix \"foo\", CamelToSnake]] Foo
--
-- instance ToSchema Foo where
--   declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions \"foo\")
-- @
mkStripPrefixSchemaOptions :: String -> SchemaOptions
mkStripPrefixSchemaOptions prefix =
  fromAesonOptions $
    AesonTypes.defaultOptions
      { AesonTypes.fieldLabelModifier =
          AesonTypes.camelTo2 '_' . dropPrefix prefix
      }
  where
    dropPrefix p s = fromMaybe s (stripPrefix p s)
