{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Either.Utils  ( maybeToEither )
import Data.Monoid        ( (<>) )
import Data.Text
import Data.Text.Encoding ( encodeUtf8 )
import Data.Time
import Database.PostgreSQL.Simple.Time
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration.Participant      ( Participant(..) )
import FarmRadio.Uliza.Registration.RegistrationCall ( RegistrationCall(..) )

import qualified FarmRadio.Uliza.Registration.RegistrationCall as RegistrationCall

-- | Look up a 'Participant' by phone number.
getParticipantByPhoneNumber :: String -> Api (Maybe Participant)
getParticipantByPhoneNumber = lookupResource "participants" "phone_number"

-- | Look up the most recent 'RegistrationCall' for a 'Participant' (if any).
getRegistrationCall :: Participant -> Api (Maybe RegistrationCall)
getRegistrationCall Participant{..} = join <$> sequence call
  where
    call = getRegistrationCallById <$> registrationCallId 

-- | Look up a 'RegistrationCall' by id.
getRegistrationCallById :: Int -> Api (Maybe RegistrationCall)
getRegistrationCallById = lookupResource "registration_calls" "id" . show 

-- | Update a 'Participant'.
patchParticipant :: Int -> Value -> Api ()
patchParticipant = fmap fmap fmap void (patchResource "participants")

-- | Post a participant to the API.
postParticipant :: Text -> Api (Maybe Participant)
postParticipant phone = do
    post_ "/participants" (object participant)
  where
    participant = [ ("phone_number"        , String phone)
                  , ("registration_status" , "NOT_REGISTERED") ]

-- | Parse and translate the 'RegistrationCall' datetime field to 'UTCTime'.
registrationCallScheduleTime :: RegistrationCall -> Maybe UTCTime
registrationCallScheduleTime RegistrationCall{..} = 
    eitherToMaybe $ parseUTCTime (encodeUtf8 scheduledTime)

-- | Post a registration call status change log entry to the API.
createStatusChangeLogEntry :: Int -> Int -> Text -> Api () 
createStatusChangeLogEntry user call event = 
    void $ post "/participant_registration_status_log" (object entry)
  where
    entry = [ ("participant_id"       , number user) 
            , ("registration_call_id" , number call) 
            , ("event_type"           , String event) ]

-- | Post a registration call to the API.
postRegistrationCall :: Text -> Text -> Api (Maybe RegistrationCall)
postRegistrationCall phone stime = post_ "/registration_calls" (object call)
  where
    call = [ ("phone_number"   , String phone)
           , ("scheduled_time" , String stime) ]

-- | Process request body and lookup or create a participant based on the
--   provided subscriber_phone property.
lookupParticipant :: Value -> Api Participant
lookupParticipant request = do

    -- Extract phone number from request 
    phone <- maybeToEither BadRequestError (extractString "subscriber_phone" request) 

    -- Log the raw JSON payload received
    void $ post "/voto_response_data" $ object [("data", request)] 

    logDebugJSON "incoming_response" request

    -- Look up participant from subscriber's phone number
    response <- getParticipantByPhoneNumber (unpack phone)

    logDebugJSON "lookup_participant" response

    case response of
      Just participant -> do
          --
          -- Participant exists: Done!  
          --
          logDebugJSON "participant_found" participant

          right participant

      Nothing -> do
          --
          -- Create a participant if one wasn't found
          --
          participant <- postParticipant phone 

          logDebugJSON "participant_created" participant

          maybeToEither (UnexpectedResponse "postParticipant") participant

-- | Data type representation of a participant's registration status
data RegistrationStatus = 
     AlreadyRegistered       -- ^ The participant is already registered
   | RegistrationDeclined    -- ^ Participant has declined to register
   | PriorCallScheduled      -- ^ A registration call is already due
   | RecentCallMade          -- ^ A call was recently made
   | ScheduleCall UTCTime    -- ^ Schedule a call at the given time
  deriving (Show)

-- | Determine a participant's registration status.
determineRegistrationStatus :: Participant 
                            -> Maybe RegistrationCall 
                            -> Api RegistrationStatus
determineRegistrationStatus Participant{..} mcall = do

    -- Current time
    now <- getCurrentTime & liftIO

    -- Time of most recent registration call (or Nothing)
    let lastCall = mcall >>= registrationCallScheduleTime

        timeTreshold = 60*60*24*2

        delay = 60*10

    logDebugJSON "participant_last_call" $ case lastCall of
      Nothing   -> "No previous registration call found for this participant."
      Just time -> (show time)

    logDebugJSON "participant_registration_status" registrationStatus

    case (registrationStatus, diffUTCTime <$> lastCall <*> Just now) of
      -- Participant is already registered
      ( "REGISTERED"     , _ ) -> right AlreadyRegistered
      -- Participant has previously declined to register
      ( "DECLINED"       , _ ) -> right RegistrationDeclined
      -- Participant is not registered
      ( "NOT_REGISTERED" , Just diff ) 
        -- A call is already scheduled
        | diff > 0    -> right PriorCallScheduled
        -- A registration call took place recently
        | diff > -timeTreshold -> right RecentCallMade
      -- No previous call was made, or last call was a while ago--let's schedule 
      ( "NOT_REGISTERED" , _ ) -> right $ ScheduleCall (addUTCTime delay now)
      -- Bad registration status
      ( _                , _ ) -> left InternalServerError

-- | Schedule a registration call for the participant at the given time.
scheduleRegistrationCall :: Participant -> UTCTime -> Api RegistrationCall
scheduleRegistrationCall Participant{ entityId = participantId, .. } time = do

    -- Send registration call to API for posterity
    regc <- postRegistrationCall phoneNumber (utcToText time)
            >>= maybeToEither (UnexpectedResponse "postRegistrationCall")

    call <- maybeToEither (UnexpectedResponse "scheduleRegistrationCall: \ 
                                              \registration_call id is null")
                          (RegistrationCall.entityId regc) 

    user <- maybeToEither (UnexpectedResponse "scheduleRegistrationCall: \
                                              \participant id is null") 
                           participantId

    -- Update the participant's registration_call_id
    patchParticipant user $ object [("registration_call_id", number call)]

    logNoticeJSON "patch_participant" ("update registration_call_id to " <> show call)

    -- Create a log entry to record the status change 
    createStatusChangeLogEntry user call "REGISTRATION_CALL_SCHEDULED"

    logNoticeJSON "registration_call_scheduled" regc

    return regc
