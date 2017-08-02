{-# LANGUAGE RecordWildCards #-}
module FarmRadio.Uliza.Registration where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Lens
import Data.Text
import Data.Text.Encoding ( encodeUtf8 )
import Data.Time
import Database.PostgreSQL.Simple.Time
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration.Participant      ( Participant(..) )
import FarmRadio.Uliza.Registration.RegistrationCall ( RegistrationCall(..) )

import qualified FarmRadio.Uliza.Registration.RegistrationCall as RegistrationCall

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
    phone <- hoist (extractString "subscriber_phone" request) BadRequestError

    -- Log the raw response
    void $ post "/voto_response_data" $ object [("data", request)] 

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

    case (registrationStatus, diffUTCTime <$> scheduleTime <*> Just now) of
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

