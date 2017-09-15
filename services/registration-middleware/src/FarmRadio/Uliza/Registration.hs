{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module FarmRadio.Uliza.Registration
  ( AppState(..)
  , AppConfig(..)
  , APIException(..)
  , RegistrationError(..)
  , RegistrationHandler
  , FarmRadio.Uliza.Registration.port
  , FarmRadio.Uliza.Registration.ulizaApi
  , FarmRadio.Uliza.Registration.votoApi
  , FarmRadio.Uliza.Registration.logLevel
  , FarmRadio.Uliza.Registration.scheduleOffset
  , FarmRadio.Uliza.Registration.callMinDelay
  , FarmRadio.Uliza.Registration.connections
  , FarmRadio.Uliza.Registration.config
  , FarmRadio.Uliza.Registration.session
  , FarmRadio.Uliza.Registration.requestBody
  , FarmRadio.Uliza.Registration.params
  , FarmRadio.Uliza.Registration.wreqOptions
  , ulizaApiPost
  , ulizaApiPost_
  , ulizaApiGet
  , ulizaApiGetOne
  , runRegistrationHandler
  ) where

import Control.Exception.Safe
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
import Network.HTTP.Client              ( HttpExceptionContent(..)
                                        , HttpException(..) )
import Network.WebSockets               ( Connection )
import Network.Wreq
import Network.Wreq.Session             ( Session )
import Network.Wreq.Types
import System.Log.Logger                ( Priority )

import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as BL
import qualified FarmRadio.Uliza.Api.Client as ApiClient

-- | Configuration data picked up from the system's environment variables.
data AppConfig = AppConfig
  { _port            :: !Int
  -- ^ (PORT) Port that the server should bind to
  , _ulizaApi        :: !String
  -- ^ (ULIZA_API_URL) Uliza API base url
  , _votoApi         :: !String
  -- ^ (VOTO_API_URL) VOTO API base url
  , _logLevel        :: !Priority
  -- ^ (LOG_LEVEL) Verbosity level of the debug output
  , _scheduleOffset  :: !NominalDiffTime
  -- ^ (CALL_SCHEDULE_OFFSET) Time to wait before scheduling a registration call
  --   (in seconds)
  , _callMinDelay    :: !NominalDiffTime
  -- ^ (MIN_RESCHEDULE_DELAY) Minimum time that must elapse between registration
  --   calls (in seconds)
  }

makeLenses ''AppConfig

-- | Application state which is accessible from within response handlers.
data AppState = AppState
  { _connections :: ![Connection]
  -- ^ A list of active WebSocket connections
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

-- | An error that can occur while running a 'RegistrationHandler'.
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

-- | Type of exceptions that can occur inside a 'RegistrationHandler'.
newtype APIException = UlizaAPIException HttpException
  deriving (Show, Typeable)

instance Exception APIException

ulizaApiException :: HttpException -> RegistrationHandler (Maybe a)
ulizaApiException = throw . UlizaAPIException

-- | Send a POST request to the Uliza API and return a JSON response.
ulizaApiPost :: (Postable a, ToJSON a, FromJSON b)
             => String -- ^ An Uliza API endpoint
             -> a      -- ^ The 'Postable' request body
             -> RegistrationHandler (Maybe b)
ulizaApiPost endpoint body = handle ulizaApiException $ do
    state <- State.get
    url <- ulizaUrl endpoint
    ApiClient.post (state ^. wreqOptions)
                   (state ^. session)
                   (resourceUrl url [])
                   body & liftIO
    >>= parseResponse

-- | Identical to 'ulizaApiPost', except that the response is ignorded.
ulizaApiPost_ :: (Postable a, ToJSON a)
              => String                 -- ^ An Uliza API endpoint
              -> a                      -- ^ The 'Postable' request body
              -> RegistrationHandler () -- ^ A unit value is returned
ulizaApiPost_ endpoint body = void post
  where
    post = ulizaApiPost endpoint body :: RegistrationHandler (Maybe Value)

-- | Send a GET request to the Uliza API and return a JSON response.
ulizaApiGet :: FromJSON a
            => String
            -- ^ An Uliza API endpoint
            -> [(String, String)]
            -- ^ A list of query string parameters as key-value pairs.
            -> RegistrationHandler (Maybe a)
ulizaApiGet endpoint params = do
    state <- State.get
    url <- ulizaUrl endpoint
    ApiClient.get (state ^. wreqOptions)
                  (state ^. session)
                  (resourceUrl url params) & liftIO
    >>= parseResponse

-- | Send a GET request to the Uliza API for a specific resouce instance, and
--   return a JSON response, which must be an object.
ulizaApiGetOne :: FromJSON a
               => String
               -- ^ An Uliza API endpoint
               -> [(String, String)]
               -- ^ A list of query string parameters as key-value pairs.
               -> Int
               -- ^ The resource id
               -> RegistrationHandler (Maybe a)
ulizaApiGetOne endpoint params pk = ulizaApiGet resource params
  where
    resource = endpoint <> "/" <> show pk

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
