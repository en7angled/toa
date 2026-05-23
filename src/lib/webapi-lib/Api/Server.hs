{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Top-level Servant API type, Swagger generation, and WAI app assembly.
module Api.Server
  ( API,
    api,
    apiSwagger,
    server,
    mkApp,
  )
where

import Api.AppMonad (AppContext (..), AppMonad, runAppMonad)
import Api.Handlers.Toa (handleDerive, handleDeriveBulk, handleDeriveTrace, handleSpend, handleUtxos)
import Api.Handlers.Tx (handleSign, handleSubmit, handleTxStatus)
import Api.Types
import Control.Lens ((&), (.~), (?~))
import Data.Swagger (Swagger, description, info, license, title, version)
import Data.Swagger qualified as Sw
import Data.Text (Text)
import GeniusYield.Types (GYTxId)
import Servant
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import TxBuilding.Interactions (AddWitAndSubmitParams, Interaction)
import WebAPI.Auth (AuthUser, basicAuthServerContext, proxyBasicAuthContext)
import WebAPI.CORS (setupCors)
import WebAPI.ServiceProbe (ServiceProbe, alwaysHealthy, alwaysReady)
import WebAPI.Utils (addSharedSwaggerDescriptions)

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

type ToaAPI =
  "toa"
    :> ( ( Summary "Derive TOA address"
             :> Description "Derive the TOA enterprise script address from (policy_id, asset_name)."
             :> "derive"
             :> QueryParam "policy_id" Text
             :> QueryParam "asset_name" Text
             :> Get '[JSON] DeriveResponse
         )
           :<|> ( Summary "Derive TOA address with full trace"
                    :> Description "Returns the full TOA derivation trace (template hash, params CBOR, applied script CBOR + hash, fingerprint, address). Used by the developer panel to cross-check against the committed CIP test vectors. Accepts an optional toa_version query param (default 1)."
                    :> "derive"
                    :> "trace"
                    :> QueryParam "policy_id" Text
                    :> QueryParam "asset_name" Text
                    :> QueryParam "toa_version" Integer
                    :> Get '[JSON] DeriveTraceResponse
                )
           :<|> ( Summary "Derive TOA addresses in bulk"
                    :> Description "Derive multiple TOA addresses in a single request."
                    :> "derive"
                    :> "bulk"
                    :> ReqBody '[JSON] [BulkDeriveItem]
                    :> Post '[JSON] [BulkDeriveResponseItem]
                )
           :<|> ( Summary "List UTxOs at a TOA address"
                    :> Description "Returns the UTxO set and aggregate balance at the given TOA address."
                    :> "utxos"
                    :> QueryParam "address" Text
                    :> Get '[JSON] UtxosResponse
                )
           :<|> ( Summary "Build a TOA spend transaction"
                    :> Description "Builds an unsigned tx that spends from a TOA. Returns hex-encoded CBOR for the wallet to sign. Request body is an 'Interaction' carrying the SpendToaAction and the wallet's UserAddresses."
                    :> "spend"
                    :> ReqBody '[JSON] Interaction
                    :> Post '[JSON] TxCborResponse
                )
       )

type TxAPI =
  "tx"
    :> ( ( Summary "Submit signed transaction"
             :> Description "Submits a wallet-signed transaction CBOR to the network."
             :> "submit"
             :> ReqBody '[JSON] SubmitRequest
             :> Post '[JSON] SubmitResponse
         )
           :<|> ( Summary "Combine unsigned tx with wallet witness"
                    :> Description "Accepts the unsigned tx body and a wallet-produced witness set, combines them server-side via @makeSignedTransaction@, and returns the resulting hex-encoded CBOR. The client submits via the wallet (CIP-30 @submitTx@) — this keeps the backend off the chain-submission path."
                    :> "sign"
                    :> ReqBody '[JSON] AddWitAndSubmitParams
                    :> Post '[JSON] TxCborResponse
                )
           :<|> ( Summary "Poll transaction status"
                    :> Description "Returns whether a transaction has been confirmed on-chain. Single-attempt check designed for client-driven polling."
                    :> "tx-status"
                    :> Capture "txId" GYTxId
                    :> Get '[JSON] TxStatusResponse
                )
       )

type PublicAPI =
  ServiceProbe Text Text
    :<|> ToaAPI
    :<|> TxAPI

type ProtectedAPI =
  BasicAuth "toa" AuthUser :> (ToaAPI :<|> TxAPI)

type API =
  SwaggerSchemaUI "swagger-ui" "swagger-api.json"
    :<|> ServiceProbe Text Text
    :<|> ProtectedAPI

api :: Proxy API
api = Proxy

publicApi :: Proxy PublicAPI
publicApi = Proxy

protectedApi :: Proxy ProtectedAPI
protectedApi = Proxy

probeApi :: Proxy (ServiceProbe Text Text)
probeApi = Proxy

-------------------------------------------------------------------------------
-- Swagger
-------------------------------------------------------------------------------

apiSwagger :: Swagger
apiSwagger =
  toSwagger publicApi
    & info . title .~ "TOA Demo API"
    & info . version .~ "0.1.0"
    & info . description ?~ "Token-Owned Addresses (TOA v1) reference HTTP API. Build deposits and spends, derive TOA addresses, list UTxOs, submit signed transactions."
    & info . license ?~ ("MIT" & Sw.url ?~ Sw.URL "https://opensource.org/licenses/MIT")
    & addSharedSwaggerDescriptions

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

probeServer :: ServerT (ServiceProbe Text Text) AppMonad
probeServer = alwaysHealthy "0.1.0" "toa-api" :<|> alwaysReady "0.1.0" "toa-api"

toaServer :: ServerT ToaAPI AppMonad
toaServer =
  handleDerive
    :<|> handleDeriveTrace
    :<|> handleDeriveBulk
    :<|> handleUtxos
    :<|> handleSpend

txServer :: ServerT TxAPI AppMonad
txServer = handleSubmit :<|> handleSign :<|> handleTxStatus

protectedServer :: ServerT ProtectedAPI AppMonad
protectedServer = const (toaServer :<|> txServer)

server :: AppContext -> Server API
server ctx =
  swaggerSchemaUIServer apiSwagger
    :<|> hoistServerWithContext probeApi proxyBasicAuthContext (runAppMonad ctx) probeServer
    :<|> hoistServerWithContext protectedApi proxyBasicAuthContext (runAppMonad ctx) protectedServer

mkApp :: AppContext -> Application
mkApp ctx =
  setupCors $
    serveWithContext api (basicAuthServerContext (authContext ctx)) (server ctx)
