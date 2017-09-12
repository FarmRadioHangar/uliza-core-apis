{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module FarmRadio.Uliza.Registration where

import Control.Lens
import Control.Monad                    ( void )
import Control.Monad.IO.Class
import Control.Monad.State              ( StateT, evalStateT )
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Maybe                       ( fromMaybe )
import Data.Monoid
import Data.Time
import Data.URLEncoded
import Network.HTTP.Base                ( urlEncodeVars )
import Network.WebSockets               ( Connection )
import Network.Wreq
import Network.Wreq.Session             ( Session )
import Network.Wreq.Types
import System.Log.Logger                ( Priority )

import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as BL
import qualified FarmRadio.Uliza.Api.Client as ApiClient

data AppConfig = AppConfig
  { _port            :: !Int
  -- ^ Port that the server should bind to
  , _ulizaApi        :: !String
  -- ^ Uliza API base url
  , _votoApi         :: !String
  -- ^ VOTO API base url
  , _logLevel        :: !Priority
  -- ^ Verbosity level of the debug output
  , _scheduleOffset  :: !NominalDiffTime
  -- ^ Time to wait before scheduling a registration call (in seconds)
  , _callMinDelay    :: !NominalDiffTime
  -- ^ Minimum time that must elapse between registration calls (in seconds)
  }

makeLenses ''AppConfig

data AppState = AppState
  { _connections :: ![Connection]
  -- ^ WebSocket connections
  , _config      :: !AppConfig
  -- ^ App configuration
  , _session     :: !Session
  -- ^ A Network.Wreq session that can handle multiple requests
  , _requestBody :: !BL.ByteString
  -- ^ Raw request body
  , _params      :: !URLEncoded
  -- ^ URL-encoded request data
  , _wreqOptions :: !Options
  -- ^ Network.Wreq connection options
  }

makeLenses ''AppState

data RegistrationError
  = UlizaApiError !String
  -- ^ Error talking to Uliza API
  | VotoApiError !String
  -- ^ Error talking to VOTO API
  | BadRequestError
  -- ^ Bad request format
  | InternalServerError !String
  -- ^ Something went wrong during processing

-- TODO
-- (UnexpectedResponse _)
-- (StatusCodeResponse code)
-- ServerConnectionFailed
-- AuthenticationError
-- NotFoundError

type RegistrationHandler = EitherT RegistrationError (StateT AppState IO)

runRegistrationHandler :: RegistrationHandler a
                       -> AppState
                       -> IO (Either RegistrationError a)
runRegistrationHandler = evalStateT . runEitherT

ulizaApiPost :: (Postable a, ToJSON a, FromJSON b)
             => String
             -> a
             -> RegistrationHandler (Maybe b)
ulizaApiPost endpoint body = do
    state <- State.get
    url <- ulizaUrl endpoint
    ApiClient.post (state ^. wreqOptions)
                   (state ^. session)
                   (resourceUrl url [])
                   body & liftIO
    >>= parseResponse

-- | Identical to 'ulizaApiPost', except that the response is ignorded.
ulizaApiPost_ :: (Postable a, ToJSON a)
              => String
              -> a
              -> RegistrationHandler ()
ulizaApiPost_ endpoint body = do
    url <- ulizaUrl endpoint
    void (ulizaApiPost url body :: RegistrationHandler (Maybe Value))

ulizaApiGet :: FromJSON a
            => String
            -> [(String, String)]
            -> RegistrationHandler (Maybe a)
ulizaApiGet endpoint params = do
    state <- State.get
    url <- ulizaUrl endpoint
    ApiClient.get (state ^. wreqOptions)
                  (state ^. session)
                  (resourceUrl url params) & liftIO
    >>= parseResponse

ulizaApiGetOne :: FromJSON a
               => String
               -> [(String, String)]
               -> Int
               -> RegistrationHandler (Maybe a)
ulizaApiGetOne endpoint params pk =
    one <$> ulizaApiGet (endpoint <> "/" <> show pk) queryParams
  where
    queryParams = ("limit", "1") : params
    one (Just [item]) = item
    one _             = Nothing

ulizaUrl :: String -> RegistrationHandler String
ulizaUrl url = do
    state <- State.get
    return (state ^. config . ulizaApi <> url)

resourceUrl :: String -> [(String, String)] -> String
resourceUrl url = \case
    []     -> url
    params -> url <> "?" <> urlEncodeVars params

parseResponse :: FromJSON a
              => Response BL.ByteString
              -> RegistrationHandler (Maybe a)
parseResponse response =
    case response ^. responseStatus . statusCode of
      200 -> ok                          -- 200 OK
      201 -> ok                          -- 201 CREATED
      202 -> ok                          -- 202 ACCEPTED
      204 -> ok                          -- 204 NO CONTENT
--      401 -> left AuthenticationError
--      404 -> left NotFoundError
--      500 -> left $ InternalServerError (Data.ByteString.Lazy.Char8.unpack body)
      err -> left (UlizaApiError "TODO")
  where
    ok = right $ decode (response ^. responseBody)
