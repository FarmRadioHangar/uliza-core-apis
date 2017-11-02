{-# LANGUAGE RecordWildCards   #-}
module FarmRadio.Uliza.Api.Participant where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Either.Utils                              ( maybeToEither )
import Data.Monoid
import Data.Text.Encoding                             ( encodeUtf8 )
import Data.Time
import Database.PostgreSQL.Simple.Time
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Logger
import FarmRadio.Uliza.Registration.Participant
import FarmRadio.Uliza.Registration.RegistrationCall

import qualified Data.Text                            as Text

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

-- | Parse and translate the 'RegistrationCall' datetime field to a 'UTCTime'.
registrationCallScheduleTime :: RegistrationCall -> Maybe UTCTime
registrationCallScheduleTime RegistrationCall{..} =
    eitherToMaybe $ parseUTCTime (encodeUtf8 scheduledTime)

