{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FarmRadio.Uliza.Registration.Voto.ResponseHandler
  ( votoResponse
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Either.Utils                              ( maybeToEither )
import Data.Monoid
import Data.Text
import Data.Text.Encoding                             ( encodeUtf8 )
import Data.Time
import Database.PostgreSQL.Simple.Time
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Logger
import FarmRadio.Uliza.Registration.Participant
import FarmRadio.Uliza.Registration.RegistrationCall

import qualified Data.Text                            as Text
import qualified Data.ByteString.Lazy.Char8           as B8
import qualified Data.URLEncoded                      as URLEncoded

-- | Response handler for survey response webhook -- triggered by VOTO every
--   time a survey response is recorded.
votoResponse :: RegistrationHandler Value
votoResponse = do
    state <- get
    let body = state ^. requestBody
    -- Log raw VOTO webhook request object
    logDebug "incoming_response" (B8.unpack body) & liftIO
    ulizaApiPost_ "/voto_webhook_log" $ object
      [ ("data"     , String (toText body))
      , ("endpoint" , "responses") ]
    let subscriber = URLEncoded.lookup ("subscriber_phone" :: String)
                                       (state ^. params)
    phone <- maybeToEither BadRequestError subscriber
    -- If the phone number is not already associated with a participant in the
    -- database, one is created here
    participant <- getOrCreateParticipant phone
    -- Find most recent registration call (if one exists) for this participant
    call <- getRegistrationCall participant
    -- Determine participant's registration status
    status <- determineRegistrationStatus participant call
    -- Take some action, then log and respond
    case status of
      AlreadyRegistered    -> noop "ALREADY_REGISTERED"
      RegistrationDeclined -> noop "REGISTRATION_DECLINED"
      PriorCallScheduled   -> noop "PRIOR_CALL_SCHEDULED"
      RecentCallMade       -> noop "TOO_SOON"
      ScheduleCall time    -> do
        call <- scheduleRegistrationCall participant time
        return $ toJSON $ object
          [ ("action", "REGISTRATION_CALL_SCHEDULED")
          , ("registration_call", toJSON call) ]
  where
    -- Request successfully processed, but a registration call was not scheduled
    noop :: Text -> RegistrationHandler Value
    noop reason = do
      liftIO $ logNotice "no_call_scheduled"
        ("A registration call was NOT scheduled. Reason: " <> unpack reason)
      return $ object $ fmap String <$> [ ("reason" , reason)
                                        , ("action" , "NONE") ]

getOrCreateParticipant :: String -> RegistrationHandler Participant
getOrCreateParticipant [] = left BadRequestError
getOrCreateParticipant ('+':phone) = getOrCreateParticipant phone
getOrCreateParticipant phone = do
    -- Look up participant from subscriber's phone number
    response <- ulizaApiGet "/participants" [ ("limit", "1")
                                            , ("subscriber_phone", phone) ]
    case response of
      Just [participant] -> do -- Participant exists: Done!
        logDebugJSON "participant_found" participant & liftIO
        right participant
      _ -> do -- Create a participant if one wasn't found
        participant <- postParticipant phone
        logDebugJSON "participant_created" participant & liftIO
        maybeToEither err participant
  where
    err = UlizaApiError "getOrCreateParticipant: Unexpected response"

postParticipant :: String -- ^ Phone number
                -> RegistrationHandler (Maybe Participant)
postParticipant phone = ulizaApiPost "/participants" (object participant)
  where
    participant = [ ("phone_number"        , String (Text.pack phone))
                  , ("registration_status" , "NOT_REGISTERED") ]

postRegistrationCall :: Text -- ^ Phone number
                     -> Text -- ^ Schedule time
                     -> RegistrationHandler (Maybe RegistrationCall)
postRegistrationCall phone time = ulizaApiPost "/registration_calls" (object call)
  where
    call = [ ("phone_number"   , String phone)
           , ("scheduled_time" , String time) ]

-- | Look up the most recent 'RegistrationCall' for a participant.
getRegistrationCall :: Participant
                    -> RegistrationHandler (Maybe RegistrationCall)
getRegistrationCall Participant{..} = join <$> sequence call
  where
    call = getRegistrationCallById <$> registrationCallId

-- | Look up a 'RegistrationCall' by id.
getRegistrationCallById :: Int -> RegistrationHandler (Maybe RegistrationCall)
getRegistrationCallById = ulizaApiGetOne "/registration_calls" []

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
                            -> RegistrationHandler RegistrationStatus
determineRegistrationStatus Participant{..} mcall = do
    state <- get
    now <- getCurrentTime & liftIO -- Current time
    let lastCall = mcall >>= registrationCallScheduleTime
        -- Time of most recent registration call (or Nothing)
        minDelay = state ^. config . callMinDelay
        offset   = state ^. config . scheduleOffset
    liftIO $ do
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
        -- A call is already scheduled for this participant
        | diff > 0             -> right PriorCallScheduled
        -- A registration call took place recently
        | diff > -minDelay     -> right RecentCallMade
      -- No previous call was made, or last call was a while ago--let's schedule
      ( "NOT_REGISTERED" , _ ) -> right $ ScheduleCall (addUTCTime offset now)
      -- Bad registration status
      ( r, _ ) -> left $ InternalServerError ("Bad registration status: " <> show r)

-- | Parse and translate the 'RegistrationCall' datetime field to 'UTCTime'.
registrationCallScheduleTime :: RegistrationCall -> Maybe UTCTime
registrationCallScheduleTime RegistrationCall{..} =
    eitherToMaybe $ parseUTCTime (encodeUtf8 scheduledTime)

-- | Schedule a registration call for a participant at a given time.
scheduleRegistrationCall :: Participant
                         -- ^ The participant to call
                         -> UTCTime
                         -- ^ Time when the registration call is to be made
                         -> RegistrationHandler RegistrationCall
scheduleRegistrationCall Participant{ entityId = participantId, .. } time =
    -- Post registration call to Uliza API
    postRegistrationCall phoneNumber (utcToText time)
    >>= maybeToEither err
    >>= \call -> logNoticeJSON "registration_call_scheduled" call & liftIO
     >> return call
  where
    err = UlizaApiError "scheduleRegistrationCall: Unexpected response"
