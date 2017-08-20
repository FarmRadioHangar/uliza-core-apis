{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Either.Utils                ( maybeToEither )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text, unpack )
import Data.Text.Encoding               ( decodeUtf8 )
import Data.Text.Format                 ( Format, Only(..), format )
import Data.Text.Lazy                   ( toStrict )
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Registration
import Network.HTTP.Types
import Network.Wai.Handler.Warp         ( run )
import Network.Wai.Handler.WebSockets
import Network.WebSockets               
import System.Log.Logger
import Web.Scotty                       ( ScottyM, scotty, status )

import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

main :: IO ()
main = do
    state <- newMVar []
    updateGlobalLogger loggerNamespace (setLevel DEBUG)
    httpServer <- Scotty.scottyApp (app state)
    let server = websocketsOr defaultConnectionOptions (ws state) httpServer
    run 3034 server

app :: MVar [Connection] -> Scotty.ScottyM ()
app state = 
    Scotty.post "/responses" (runHandler votoResponse)
--    Scotty.post "/call_status_update" (runHandler callStatusUpdate)

ws :: MVar [Connection] -> ServerApp
ws state pending = do
    conn <- acceptRequest pending
    modifyMVar_ state $ \conns -> return (conn : conns)
    sendTextData conn ("Hello, client!" :: Text)

runHandler :: (Value -> Api Value) -> Scotty.ActionM ()
runHandler handler = 
    decode <$> Scotty.body 
    >>= liftIO . runApi . action
    >>= either errorResponse jsonResponse 
  where 
    action body = do
      setBaseUrl "http://localhost:8000/api/v1" -- setOauth2Token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiYXBwIn0.SUkjmtzmR6xYenoihVFKMl_XTdmawTnQhsDSj7yeTH8"
      setHeader "Accept" ["application/json"]
      setHeader "User-Agent" ["Uliza VOTO Registration Middleware"]
      maybeToEither BadRequestError body >>= handler

-- Normal response
jsonResponse :: Value -> Scotty.ActionM ()
jsonResponse value = status ok200 >> Scotty.json value

-- An error occurred
errorResponse :: ApiError -> Scotty.ActionM ()
errorResponse err = do
    liftIO (errorM loggerNamespace $ "[server_error] " <> logErrorMessage err)
    status internalServerError500
    Scotty.json $ object [("error", String (hint err))]

votoResponse :: Value -> Api Value
votoResponse request = do

    -- Log raw VOTO survey response
    logDebugJSON "incoming_response" request
    post "/voto_response_data" $ object [("data", String (toText request))]

    -- Get or create participant
    participant <- getOrCreateParticipant (FromRequest request)

    -- Retrieve most recent registration call for participant (if any) 
    call <- getRegistrationCall participant

    -- Determine participant's registration status
    status <- determineRegistrationStatus participant call

    -- Take some action, log and respond
    case status of
      AlreadyRegistered    -> noAction "PARTICIPANT_ALREADY_REGISTERED"
      RegistrationDeclined -> noAction "REGISTRATION_DECLINED"
      PriorCallScheduled   -> noAction "PRIOR_CALL_SCHEDULED"
      RecentCallMade       -> noAction "RECENT_CALL_MADE"
      ScheduleCall time    -> do
        call <- scheduleRegistrationCall participant time 
        return $ toJSON $ object 
          [ ("action", "REGISTRATION_CALL_SCHEDULED")
          , ("registration_call", toJSON call) ]
  where
    toText :: Value -> Text
    toText = decodeUtf8 . BL.toStrict . encode 
    noAction :: Text -> Api Value
    noAction message = do
      -- Request successfully processed, but no registration call was scheduled
      liftIO $ noticeM loggerNamespace $ "[no_call_scheduled]\
             \ A registration call was NOT scheduled. Reason: " 
            <> unpack message
      return $ object $ fmap String <$> [ ("message" , message) 
                                        , ("action"  , "NONE") 
                                        ]
callStatusUpdate :: Value -> Api Value
callStatusUpdate request = undefined

-- properties :: Value -> Maybe Value
-- properties v = v ^? key "data" 
--                   . key "subscriber" 
--                   . key "properties" 
-- 
-- callStatusUpdate :: Value -> Api Value
-- callStatusUpdate request = do
--   logDebugJSON "incoming_call_status_update" request 
--   callComplete <- isCallComplete request
--   phone  <- maybeToEither BadRequestError (extractString "subscriber_phone" request)
--   votoId <- maybeToEither BadRequestError (extractInt "subscriber_id" request)
--   subscriber <- votoSubscriber votoId & liftIO
--   print subscriber & liftIO
--   registered <- maybeToEither InternalServerError (join $ extractBool "registered" <$> properties subscriber) 
--   if callComplete && registered
--      then do
--        getOrCreateParticipant (FromPhoneNumber phone)
--          >>= registerParticipant 
--          >>= send
--      else do
--        liftIO $ noticeM loggerNamespace $ "[no_action] Participant not registered." 
--        return $ object [("message", "NO_ACTION")]
--   where
--     send response = do
--       liftIO $ noticeM loggerNamespace $ "[registration_complete]\ 
--                                        \ Participant registration complete." 
--       return $ object 
--         [ ("message" , "REGISTRATION_COMPLETE")
--         , ("data"    , response) ]

hint :: ApiError -> Text
hint InternalServerError       = "INTERNAL_SERVER_ERROR"
hint (UnexpectedResponse _)    = "UNEXPECTED_RESPONSE_FORMAT"
hint (StatusCodeResponse code) = "STATUS_CODE_{}" & statusCodeResponse code 
hint ServerConnectionFailed    = "SERVER_CONNECTION_FAILED"
hint AuthenticationError       = "UNAUTHORIZED"
hint NotFoundError             = "API_RESOURCE_NOT_FOUND"
hint BadRequestError           = "BAD_REQUEST_FORMAT"

statusCodeResponse :: Int -> Format -> Text
statusCodeResponse code = toStrict . flip format (Only code) 

logErrorMessage :: ApiError -> String
logErrorMessage InternalServerError = "Server error."
logErrorMessage (UnexpectedResponse what)
  = "An unexpected response was received from the API server. (" <> what <> ")"
logErrorMessage (StatusCodeResponse code) 
  = "The API server responded with status code " <> show code
logErrorMessage ServerConnectionFailed    
  = "Connection failed. Is the API server running?"
logErrorMessage AuthenticationError       
  = "Authentication error when connecting to API server.\
   \ Verify that the JSON web token (JWT) is valid."
logErrorMessage NotFoundError 
  = "The API server returned a 404 error. Check the database schema!"
logErrorMessage BadRequestError = "The request format was not recognized."
