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
import FarmRadio.Uliza.Registration.Voto.CallScheduleResponse

scheduleVotoCall :: Text -- ^ Phone number
                 -> RegistrationHandler (Maybe CallScheduleResponse)
scheduleVotoCall phone = do
    votoApiPost "/outgoing_calls" (object call)
  where
    call = [ ("send_to_phones" , String phone) ]

postRegistrationCall :: Text -- ^ Phone number
                     -> Text -- ^ Schedule time
                     -> RegistrationHandler (Maybe RegistrationCall)
postRegistrationCall phone time = ulizaApiPost "/registration_calls" (object call)
  where
    call = [ ("phone_number"   , String phone)
           , ("scheduled_time" , String time) ]

-- | Request the most recent 'RegistrationCall' for a participant from the
--   Uliza API.
getRegistrationCall :: Participant
                    -> RegistrationHandler (Maybe RegistrationCall)
getRegistrationCall Participant{..} = join <$> sequence call
  where
    call = getRegistrationCallById <$> registrationCallId

-- | Request the 'RegistrationCall' with the given id from the Uliza API.
getRegistrationCallById :: Int -> RegistrationHandler (Maybe RegistrationCall)
getRegistrationCallById = ulizaApiGetOne "/registration_calls" []

-- | Schedule a registration call for a participant at a given time.
scheduleRegistrationCall :: Participant
                         -- ^ The participant to call
                         -> UTCTime
                         -- ^ Time when the registration call is to be made
                         -> RegistrationHandler RegistrationCall
scheduleRegistrationCall Participant{ entityId = participantId, .. } time = do
    -- Schedule an outgoing call with VOTO
    response <- scheduleVotoCall phoneNumber
    print response & liftIO 
    -- Post registration call to Uliza API
    postRegistrationCall phoneNumber (utcToText time)
    >>= maybeToEither err
    >>= \call -> logNoticeJSON "registration_call_scheduled" call & liftIO
     >> return call
  where
    err = UlizaApiError "scheduleRegistrationCall: Unexpected response"
