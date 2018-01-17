{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration.App
  ( app
  , wss
  ) where

import Control.Exception.Safe
import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Monoid
import Data.Text
import Data.Text.Encoding
import Data.URLEncoded
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Voto.ResponseHandler
import FarmRadio.Uliza.Registration.Voto.CallStatusUpdateHandler
import FarmRadio.Uliza.Registration.Logger
import Network.HTTP.Client                            ( HttpExceptionContent(..)
                                                      , HttpException(..) )
import Network.HTTP.Types
import Network.Wai                                    ( Request(pathInfo) )
import Network.WebSockets                             ( ServerApp
                                                      , acceptRequest
                                                      , sendTextData )
import Web.Scotty                                     ( ScottyM
                                                      , scotty
                                                      , status )

import qualified Data.ByteString.Lazy.Char8           as B8
import qualified Web.Scotty                           as Scotty
import qualified Data.Text.Lazy                       as LT

registrationHandlerException :: SomeException 
                             -> IO (Either RegistrationError a)
registrationHandlerException e =
    case fromException e of
      Just (UlizaAPIException e) -> left $ UlizaApiError (show e)
      Nothing                    -> throw e
  where
    left = return . Left

-- | HTTP server application
app :: MVar AppState -> ScottyM ()
app state = do
    Scotty.get  "/"                    undefined
    Scotty.post "/responses"           (runHandler votoResponse)
    Scotty.post "/call_status_updates" (runHandler votoCallStatusUpdate)
    Scotty.notFound                    notFound
  where
    runHandler :: RegistrationHandler Value -> Scotty.ActionM ()
    runHandler handler =
      either errorResponse jsonResponse =<< do
        body <- Scotty.body
        pars <- Scotty.params
        let encoded = importList (bimap LT.unpack LT.unpack <$> pars)
        --encoded <- importString (B8.unpack body)
        liftIO $ handle registrationHandlerException $ readMVar state
          >>= runRegistrationHandler handler
          . set params encoded
          . set requestBody body

-- | Send a 404 NOT FOUND response.
notFound :: Scotty.ActionM ()
notFound = do
    request <- Scotty.request
    liftIO $ logError "application_error" $ "No such resource: "
                   <> (unpack . intercalate "/") (pathInfo request)
    status status404
    Scotty.json $ object [("error", "NOT FOUND")]

-- | Send a normal 200 OK response.
jsonResponse :: Value -> Scotty.ActionM ()
jsonResponse value = status ok200 >> Scotty.json value

-- | Send an error response, based on the 'RegistrationError' argument.
errorResponse :: RegistrationError -> Scotty.ActionM ()
errorResponse err = do
    liftIO $ logError "application_error" (errorLogMessage err)
    status errStatus
    Scotty.json $ object [("error", message)]
  where
    errStatus = errorStatus err
    message = String $ decodeUtf8 $ statusMessage errStatus

errorLogMessage :: RegistrationError -> String
errorLogMessage (InternalServerError e)
  = "Server error while processing the request: " <> e
errorLogMessage (VotoApiError e)
  = "Error talking to the VOTO API: " <> e
errorLogMessage (UlizaApiError e)
  = "Error talking to the Uliza API: " <> e
errorLogMessage BadRequestError
  = "The request format is invalid."

errorStatus :: RegistrationError -> Status
errorStatus BadRequestError = badRequest400
errorStatus _               = internalServerError500

-- | WebSocket server application
wss :: MVar AppState -> ServerApp
wss state pending = do
    conn <- acceptRequest pending
    modifyMVar_ state $ pure . over connections (conn :)
    sendTextData conn ("Hello, client!" :: Text)
