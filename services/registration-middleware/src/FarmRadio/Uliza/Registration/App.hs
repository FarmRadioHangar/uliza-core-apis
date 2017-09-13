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
import Data.URLEncoded
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Voto.ResponseHandler
import FarmRadio.Uliza.Registration.Logger
import Network.HTTP.Client                            ( HttpExceptionContent(..)
                                                      , HttpException(..) )
import Network.HTTP.Types
import Network.WebSockets                             ( ServerApp
                                                      , acceptRequest
                                                      , sendTextData )
import Web.Scotty                                     ( ScottyM
                                                      , scotty
                                                      , status )

import qualified Data.ByteString.Lazy.Char8           as B8
import qualified Web.Scotty                           as Scotty

registrationHandlerException :: SomeException -> IO (Either RegistrationError a)
registrationHandlerException e =
    case fromException e of
      Just (UlizaAPIException e) -> return $ Left $ UlizaApiError (show e)
      Nothing                    -> throw e

-- | HTTP server application
app :: MVar AppState -> ScottyM ()
app state = do
    Scotty.get  "/"                    undefined
    Scotty.post "/responses"           (runHandler votoResponse)
    Scotty.post "/call_status_updates" undefined
  where
    runHandler :: RegistrationHandler Value -> Scotty.ActionM ()
    runHandler handler =
      either errorResponse jsonResponse =<< do
        body <- Scotty.body
        encoded <- importString (B8.unpack body)
        liftIO $ handle registrationHandlerException $ readMVar state
          >>= runRegistrationHandler handler
          . set params encoded
          . set requestBody body

jsonResponse :: Value -> Scotty.ActionM ()
jsonResponse value = status ok200 >> Scotty.json value

errorResponse :: RegistrationError -> Scotty.ActionM ()
errorResponse err = do
  liftIO $ logError "application_error" (errorLogMessage err)
  status internalServerError500
  Scotty.json $ object [("error", String (errorType err))]

errorLogMessage :: RegistrationError -> String
errorLogMessage (InternalServerError e)
  = "Server error while processing the request: " <> e
errorLogMessage (VotoApiError e)
  = "Error talking to the VOTO API: " <> e
errorLogMessage (UlizaApiError e)
  = "Error talking to the Uliza API: " <> e
errorLogMessage BadRequestError
  = "The request format is invalid."

-- | A symbolic identifier used to describe the error in the HTTP JSON response.
errorType :: RegistrationError -> Text
errorType BadRequestError = "BAD_REQUEST_FORMAT"
errorType _               = "INTERNAL_SERVER_ERROR"

-- | WebSocket server application
wss :: MVar AppState -> ServerApp
wss state pending = do
    conn <- acceptRequest pending
    modifyMVar_ state $ pure . over connections (conn :)
    sendTextData conn ("Hello, client!" :: Text)
