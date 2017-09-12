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

httpException :: HttpException -> IO (Either RegistrationError a)
httpException e = print e >> return (Left BadRequestError) -- TODO

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
        liftIO $ handle httpException $ readMVar state
          >>= runRegistrationHandler handler
          . set params encoded
          . set requestBody body

jsonResponse :: Value -> Scotty.ActionM ()
jsonResponse value = status ok200 >> Scotty.json value

errorResponse :: RegistrationError -> Scotty.ActionM ()
errorResponse err = do
  liftIO $ logError "server_error" (errorLogMessage err)
  status internalServerError500
  Scotty.json $ object [("error", String (errorType err))]

errorLogMessage :: RegistrationError -> String
errorLogMessage (InternalServerError e) = "Server error: " <> e
errorLogMessage (VotoApiError e) = "VOTO API error: " <> e
errorLogMessage (UlizaApiError e) = "Uliza API error: " <> e
errorLogMessage BadRequestError = "The request format was not recognized."

errorType :: RegistrationError -> Text
errorType (InternalServerError _) = "INTERNAL_SERVER_ERROR"
errorType (VotoApiError _)        = "VOTO_API_ERROR"
errorType (UlizaApiError _)       = "ULIZA_API_ERROR"
errorType BadRequestError         = "BAD_REQUEST_FORMAT"

-- | WebSocket server application
wss :: MVar AppState -> ServerApp
wss state pending = do
    conn <- acceptRequest pending
    modifyMVar_ state $ pure . over connections (conn :)
    sendTextData conn ("Hello, client!" :: Text)
