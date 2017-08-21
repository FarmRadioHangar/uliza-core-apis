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
import Data.Maybe         ( isJust, fromJust )
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
import qualified Data.Text as T

-- | Look up a 'Participant' by phone number.
getParticipantByPhoneNumber :: String -> Api (Maybe Participant)
getParticipantByPhoneNumber num = 
    getWhere "participants" "phone_number" num [("limit", "1")]
      >>= \case Just [one] -> return one
                _          -> return Nothing

-- | Look up the most recent 'RegistrationCall' for a 'Participant' (if any).
getRegistrationCall :: Participant -> Api (Maybe RegistrationCall)
getRegistrationCall Participant{..} = join <$> sequence call
  where
    call = getRegistrationCallById <$> registrationCallId 

-- | Look up a 'RegistrationCall' by id.
getRegistrationCallById :: Int -> Api (Maybe RegistrationCall)
getRegistrationCallById pk = getResource "registration_calls" (show pk)

-- | Update a 'Participant'.
patchParticipant :: Int -> Value -> Api ()
patchParticipant = fmap fmap fmap void (patchResource "participants")

-- | Post a participant to the API.
postParticipant :: Text -> Api (Maybe Participant)
postParticipant phone = post_ "/participants" (object participant)
  where
    participant = [ ("phone_number"        , String phone)
                  , ("registration_status" , "NOT_REGISTERED") ]

-- | Parse and translate the 'RegistrationCall' datetime field to 'UTCTime'.
registrationCallScheduleTime :: RegistrationCall -> Maybe UTCTime
registrationCallScheduleTime RegistrationCall{..} = 
    eitherToMaybe $ parseUTCTime (encodeUtf8 scheduledTime)

---- | Post a registration call status change log entry to the API.
--createStatusChangeLogEntry :: Int -> Maybe Int -> Text -> Api () 
--createStatusChangeLogEntry user call event = 
--    void $ post "/participant_registration_status_log" (object entry)
--  where
--    entry = [ ("participant_id"       , number user) 
--            , ("registration_call_id" , maybe Null number call) 
--            , ("event_type"           , String event) ]

-- | Post registration call to the API.
postRegistrationCall :: Text -> Text -> Api (Maybe RegistrationCall)
postRegistrationCall phone stime = post_ "/registration_calls" (object call)
  where
    call = [ ("phone_number"   , String phone)
           , ("scheduled_time" , String stime) ]

-- | Look up a participant 
data LookUpParticipant
   = FromRequest     !Value -- ^ from JSON request object 
   | FromPhoneNumber !Text  -- ^ from phone number
  deriving (Show)

-- | Look up or create a participant from request object or a phone number.
getOrCreateParticipant :: LookUpParticipant -> Api Participant
getOrCreateParticipant (FromRequest request) = do

    -- Extract phone number from request 
    phone <- maybeToEither BadRequestError (extractString "subscriber_phone" request) 

    getOrCreateParticipant (FromPhoneNumber phone)

getOrCreateParticipant (FromPhoneNumber phone) 
  | T.null phone        = left BadRequestError 
  -- ^ Phone number must not be null
  | '+' == T.head phone = getOrCreateParticipant $ FromPhoneNumber $ T.tail phone
  -- ^ Strip off leading '+' char
  | otherwise           = do

    -- Look up participant from subscriber's phone number
    response <- getParticipantByPhoneNumber (unpack phone)

    case response of

      -- Participant exists: Done!  
      Just participant -> do

          logDebugJSON "participant_found" participant
          right participant

      -- Create a participant if one wasn't found
      Nothing -> do

          participant <- postParticipant phone
          logDebugJSON "participant_created" participant
          maybeToEither (UnexpectedResponse "postParticipant") participant

-- | Data type to represent a participant's registration status
data RegistrationStatus 
   = AlreadyRegistered       -- ^ The participant is already registered
   | RegistrationDeclined    -- ^ Participant has declined to register
   | PriorCallScheduled      -- ^ A registration call is already due
   | RecentCallMade          -- ^ A call was recently made
   | ScheduleCall !UTCTime   -- ^ Schedule a call at this time
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
      Just time -> show time

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

    -- Post registration call to API 
    regc  <- postRegistrationCall phoneNumber (utcToText time)
         >>= maybeToEither (UnexpectedResponse "postRegistrationCall")

    logNoticeJSON "registration_call_scheduled" regc

    return regc

registerParticipant :: Participant -> Api Value
registerParticipant participant@Participant{ entityId = participantId, .. } = do

    user <- maybeToEither (UnexpectedResponse "registerParticipant: \
                          \participant id is null") participantId

    when ("REGISTERED" == registrationStatus) $ logWarning "already_registered" 
          "Performing registration for already registered listener."

    -- Update the participant's registration_status
    patchParticipant user $ object [("registration_status", "REGISTERED")]

    return (toJSON participant { registrationStatus = "REGISTERED" })

isCallComplete :: Value -> Api Bool
isCallComplete request = do

    -- Extract delivery status
    status <- maybeToEither BadRequestError 
              (extractInt "delivery_status" request) 

    return (6 == status)
