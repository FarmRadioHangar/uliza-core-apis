{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FarmRadio.Uliza.Api.RegistrationCall where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Either.Utils                              ( maybeToEither )
import Data.Text
import Data.Time
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Logger
import FarmRadio.Uliza.Registration.Participant
import FarmRadio.Uliza.Registration.RegistrationCall

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
