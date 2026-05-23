-- | Basic authentication middleware for Servant APIs.
-- Reads credentials from @BASIC_USER@ and @BASIC_PASS@ environment variables;
-- fails fast at startup if either is unset.
module WebAPI.Auth
  ( AuthUser (..)
  , AuthContext (..)
  , proxyBasicAuthContext
  , authCheck
  , basicAuthServerContext
  , getBasicAuthFromEnv
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Servant
import System.Environment (lookupEnv)
import System.Exit (die)

-- | Authenticated user identity extracted from a successful basic-auth check.
newtype AuthUser = AuthUser
  { user :: Text
  }
  deriving (Eq, Show)

-- | Expected credentials used to validate incoming basic-auth requests.
data AuthContext = AuthContext
  { authUser :: Text
  , authPassword :: Text
  }
  deriving (Eq, Show)

proxyBasicAuthContext :: Proxy '[BasicAuthCheck AuthUser]
proxyBasicAuthContext = Proxy

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: AuthContext -> BasicAuthCheck AuthUser
authCheck AuthContext {authUser, authPassword} =
  let check (BasicAuthData username password) =
        if TE.decodeUtf8 username == authUser && TE.decodeUtf8 password == authPassword
          then return (Authorized (AuthUser authUser))
          else return Unauthorized
   in BasicAuthCheck check

-- | Build a Servant 'Context' containing the basic-auth check for use with 'serveWithContext'.
basicAuthServerContext :: AuthContext -> Context (BasicAuthCheck AuthUser ': '[])
basicAuthServerContext authContext = authCheck authContext :. EmptyContext

-- | Read basic-auth credentials from @BASIC_USER@ and @BASIC_PASS@ env vars.
-- Refuses to launch if either is unset.
getBasicAuthFromEnv :: IO AuthContext
getBasicAuthFromEnv = do
  mUser <- lookupEnv "BASIC_USER"
  mPass <- lookupEnv "BASIC_PASS"
  case (mUser, mPass) of
    (Just u, Just p) -> pure AuthContext {authUser = T.pack u, authPassword = T.pack p}
    _ -> die "BASIC_USER and BASIC_PASS environment variables must be set"
