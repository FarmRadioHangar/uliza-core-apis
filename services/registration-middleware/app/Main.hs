{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Either.Utils                ( maybeToEither )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text, unpack )
import Data.Text.Format                 ( Only(..), format )
import Data.Text.Lazy                   ( toStrict )
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Registration
import Network.HTTP.Types
import System.Log.Logger
import Web.Scotty                       ( ScottyM, scotty )

import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

main :: IO ()
main = do
    updateGlobalLogger loggerNamespace (setLevel DEBUG)
    scotty 3034 app

app :: ScottyM ()
app = Scotty.post "/responses" (Scotty.body >>= responseHandler)

responseHandler :: BL.ByteString -> Scotty.ActionM ()
responseHandler body = either errorResponse Scotty.json =<< liftIO (runApi task)
  where
    task = do
      -- 
      setBaseUrl "http://localhost:3000"
      setOauth2Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiYXBwIn0.SUkjmtzmR6xYenoihVFKMl_XTdmawTnQhsDSj7yeTH8"
      --setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXV0aCJ9.ECxpLsBiTGIUPLV75-RdwqerfS-asXAPGvAxMenbroo"
      setHeader "Accept" ["application/json"]
      setHeader "User-Agent" ["Uliza VOTO Registration Middleware"]
      --
      maybeToEither BadRequestError (decode body) 
        >>= lookupParticipant 
          >>= \user -> getRegistrationCall user
            >>= determineRegistrationStatus user 
              >>= \case 
                AlreadyRegistered    -> noAction "PARTICIPANT_ALREADY_REGISTERED"
                RegistrationDeclined -> noAction "REGISTRATION_DECLINED"
                PriorCallScheduled   -> noAction "PRIOR_CALL_SCHEDULED"
                RecentCallMade       -> noAction "RECENT_CALL_MADE"
                ScheduleCall time    -> toJSON <$> scheduleRegistrationCall user time

    -- Request successfully processed, but no registration call was scheduled
    --
    noAction :: Text -> Api Value
    noAction message = do

      liftIO $ noticeM loggerNamespace $ "[no_call_scheduled]\
             \ A registration call was NOT scheduled. Reason: " 
             <> unpack message

      return $ object [("message", String message)]

    -- An error occurred
    --
    errorResponse :: ApiError -> Scotty.ActionM ()
    errorResponse err = do
      liftIO $ errorM loggerNamespace $ "[server_error] " <> logErrorMessage err
      Scotty.status internalServerError500
      Scotty.json $ object [("error", String (hint err))]

    hint :: ApiError -> Text
    hint InternalServerError       = "INTERNAL_SERVER_ERROR"
    hint (UnexpectedResponse _)    = "UNEXPECTED_RESPONSE_FORMAT"
    hint (StatusCodeResponse code) = statusCodeResponse code "STATUS_CODE_{}" 
    hint ServerConnectionFailed    = "SERVER_CONNECTION_FAILED"
    hint AuthenticationError       = "UNAUTHORIZED"
    hint BadRequestError           = "BAD_REQUEST_FORMAT"

    logErrorMessage :: ApiError -> String
    logErrorMessage InternalServerError       
      = "Server error."
    logErrorMessage (UnexpectedResponse what)
      = "An unexpected response was received from the API server. (" <> what <> ")"
    logErrorMessage (StatusCodeResponse code) 
      = "The API server responded with status code " <> show code
    logErrorMessage ServerConnectionFailed    
      = "Connection failed. Is the API server running?"
    logErrorMessage AuthenticationError       
      = "Authentication error when connecting to API server.\
       \ Verify that the JSON web token (JWT) is valid."
    logErrorMessage BadRequestError           
      = "The request format was not recognized."

    statusCodeResponse code = toStrict . flip format (Only code) 

