module Main where

import Configuration.Dotenv                ( onMissingFile )
import Control.Concurrent
import Control.Lens
import Data.Maybe                          ( fromMaybe )
import Data.Monoid
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.App
import FarmRadio.Uliza.Registration.Logger
import Network.Wai.Handler.Warp            ( defaultSettings
                                           , runSettings
                                           , setPort
                                           , setBeforeMainLoop )
import Network.Wai.Handler.WebSockets
import Network.WebSockets                  ( defaultConnectionOptions )
import Network.Wreq                        ( defaults )
import Network.Wreq.Session
import System.Environment                  ( lookupEnv )
import System.Log.Logger

import qualified Configuration.Dotenv      as Dotenv
import qualified Web.Scotty                as Scotty

main :: IO ()
main = Dotenv.loadFile False ".env" `onMissingFile` pure ()
    >> readConfig >>= runServer

readConfig :: IO AppConfig
readConfig = do
    port     <- lookupEnv "PORT"
    ulizaApi <- lookupEnv "ULIZA_API_URL"
    votoApi  <- lookupEnv "VOTO_API_URL"
    votoKey  <- lookupEnv "VOTO_API_KEY" -- @TODO: Fail if key is missing?
    logLevel <- lookupEnv "LOG_LEVEL"
    offset   <- lookupEnv "CALL_SCHEDULE_OFFSET"
    delay    <- lookupEnv "MIN_RESCHEDULE_DELAY"
    return AppConfig
      { _port           = fromMaybe 3034  (read <$> port)
      , _logLevel       = fromMaybe DEBUG (read <$> logLevel)
      , _ulizaApi       = fromMaybe "http://localhost:8000/api/v1" ulizaApi
      , _votoApi        = fromMaybe "https://go.votomobile.org/api/v1" votoApi
      , _votoApiKey     = fromMaybe "" votoKey
      , _scheduleOffset = fromMaybe (60*10) (fromIntegral . read <$> offset)
      , _callMinDelay   = fromMaybe (60*60*24*2) (fromIntegral . read <$> delay) }

runServer :: AppConfig -> IO ()
runServer config = withAPISession $ \session -> do
    updateGlobalLogger loggerNamespace $ setLevel (config ^. logLevel)
    state <- newMVar AppState
      { _connections = mempty
      , _config      = config
      , _session     = session
      , _requestBody = mempty
      , _params      = mempty
      , _wreqOptions = defaults }
    server <- Scotty.scottyApp (app state)
    let app = websocketsOr defaultConnectionOptions (wss state) server
    runSettings settings app
  where
    settings = setPort (config ^. port) defaultSettings
             & setBeforeMainLoop (logNotice "notice"
             $ "Server listening on port " <> show (config ^. port))
