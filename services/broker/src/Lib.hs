{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
    , obj
    , hoist
    , number
    , extractString
  ) where

import Control.Exception.Safe
import Control.Lens
import Control.Monad (void, when, unless, join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State   ( StateT, runStateT, modify, withState )
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Scientific 
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import Data.Time.Format
import Database.PostgreSQL.Simple.Time
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Api.Utils
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.Wreq (Response, responseStatus, responseBody, statusCode)
import FarmRadio.Uliza.Registration.Participant (Participant(..))
import FarmRadio.Uliza.Registration.RegistrationCall (RegistrationCall(..))
import Network.HTTP.Types.Status
import Web.Scotty (ScottyM, scotty)

import qualified FarmRadio.Uliza.Registration.Participant as Participant
import qualified FarmRadio.Uliza.Registration.RegistrationCall as RegistrationCall

import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

obj :: String
obj = "{\"data\":{\"question_id\": \"123123\", \"survey_id\": \"89324\", \"voto_id\": \"44\", \"response_type\": \"1\", \"content_type\": \"1\", \"poll_id\": \"213\", \"delivery_log_id\": \"832\", \"choice_id\": \"1\", \"subscriber_id\": \"232\", \"subscriber_phone\": \"+233212323\", \"question_title\": \"adfadfadfasdfasfdafd\", \"choice_name\": \"adfadsfas fadsfadsfasdf\", \"date_received\": \"2017-07-24T18:13:51Z\"}}"

app :: ScottyM ()
app = Scotty.post "/registrations" $ do
        liftIO banan
        Scotty.text "OK!"

someFunc :: IO ()
someFunc = scotty 3034 app

banan :: IO ()
banan = do
    xx <- runApi $ do
        -- 
        setBaseUrl "http://localhost:3000"
        setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXBpX2NvbnN1bWVyIn0.pCGD-RP8oYcYzLukq1HEKyuQ2iFMPFXpPt3Aum7aXYY"
        setHeader "Accept" ["*/*"]
        setHeader "Prefer" ["return=representation"]
        --
        hoist (obj ^? key "data") BadRequestError
          >>= lookupParticipant 
          >>= \user -> getRegistrationCall user
          >>= \call -> scheduleCall user call
        --
    print xx

getParticipantByPhoneNumber :: String -> Api (Maybe Participant)
getParticipantByPhoneNumber = getSingleEntity "participants" "phone_number"

getRegistrationCall :: Participant -> Api (Maybe RegistrationCall)
getRegistrationCall Participant{..} = join <$> sequence call
  where
    call = getRegistrationCallById <$> registrationCallId 

getRegistrationCallById :: Int -> Api (Maybe RegistrationCall)
getRegistrationCallById = getSingleEntity "registration_calls" "id" . show 

patchParticipant :: Int -> Value -> Api ()
patchParticipant = fmap fmap fmap void (patchResource "participants")

postParticipant :: Text -> Api (Maybe Participant)
postParticipant phone = post_ "/participants" (object participant)
  where
    participant = [ ("phone_number"        , String phone)
                  , ("registration_status" , "NOT_REGISTERED") ]

registrationCallScheduleTime :: RegistrationCall -> Maybe UTCTime
registrationCallScheduleTime RegistrationCall{..} = 
    eitherToMaybe $ parseUTCTime (encodeUtf8 scheduleTime)

logRegistration :: Int -> Int -> Api () 
logRegistration user call = 
    void $ post "/participant_registration_status_log" (object entry)
  where
    entry = [ ("participant_id"       , number user) 
            , ("registration_call_id" , number call) 
            , ("event_type"           , "REGISTRATION_CALL_SCHEDULED") ]

postRegistrationCall :: Text -> Text -> Api (Maybe RegistrationCall)
postRegistrationCall phone stime = post_ "/registration_calls" (object call)
  where
    call = [ ("phone_number"  , String phone)
           , ("schedule_time" , String stime) ]

lookupParticipant :: Value -> Api Participant
lookupParticipant request = do

    -- Extract phone number from request object
    phone <- hoist (extractString "subscriber_phone" request) XXX

    -- Log the raw response
    post "/voto_response_data" $ object [("data", request)] 

    -- Look up participant from subscriber's phone number
    response <- getParticipantByPhoneNumber (unpack phone)

    case response of
      --
      -- Participant exists: Done!  
      --
      Just participant -> right participant
      --
      -- Create a participant if one wasn't found
      --
      Nothing -> postParticipant phone >>= flip hoist XXX

scheduleCall :: Participant -> Maybe RegistrationCall -> Api ()
scheduleCall Participant{..} mcall = do

    -- Current time
    now <- getCurrentTime & liftIO

    -- Time of most recent registration call (or Nothing)
    let scheduleTime = mcall >>= registrationCallScheduleTime

    case (registrationStatus, diffUTCTime <$> scheduleTime 
                                          <*> Just now) of
      -- Participant is already registered
      ( "REGISTERED"     , _       ) -> left XXX
      -- Participant has previously declined to register
      ( "DECLINED"       , _       ) -> left XXX
      -- Participant rs not registered
      ( "NOT_REGISTERED" , Just diff ) 
        -- A call is already scheduled
        | diff > 0    -> left XXX
        -- A registration call took place recently
        | diff > -120 -> left XXX
      -- No previous call was made or last call was long ago -- schedule a call
      ( "NOT_REGISTERED" , _       ) -> right ()
      -- Bad registration status
      ( _                , _       ) -> left XXX

    -- Create the registration call
    --
    resp <- postRegistrationCall phoneNumber $ utcToText (addUTCTime 600 now)
    user <- hoist participantId XXX
    call <- hoist (resp >>= RegistrationCall.registrationCallId) XXX

    -- Update the participant's registration_call_id
    patchParticipant user $ object [("registration_call_id", number call)]

    -- Create a REGISTRATION_CALL_SCHEDULED log entry
    logRegistration user call


--    --what :: HttpException -> IO ()
--    --what e = 
--    --  case e of
--    --    HttpExceptionRequest req exception -> 
--    --      case exception of
--    --        StatusCodeException r bs -> print (r ^. responseStatus . statusCode)
--    --    e -> print (displayException e)
