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
import FarmRadio.Uliza.Registration.SurveyTreePair
import FarmRadio.Uliza.Registration.Voto.CallScheduleResponse
import TextShow

-- | Post request to VOTO's API to schedule a registration call.
scheduleVotoCall :: Text 
                 -- ^ Phone number
                 -> Int  
                 -- ^ Tree id
                 -> RegistrationHandler (Maybe CallScheduleResponse)
                 -- ^ Return the VOTO response
scheduleVotoCall phone treeId = do
    state <- get
    logDebugJSON "voto_call_scheduled" call & liftIO
    votoApiPost "outgoing_calls" call
  where
    call = object [ ("send_to_phones" , String phone) 
                  , ("tree_id"        , String (showt treeId))
                  , ("webhook_url"    , "http://aff2984f.ngrok.io/call_status_updates")
                  , ("webhook_method" , "POST") ]

-- | Create the registration call in Uliza.
createRegistrationCall :: Text -- ^ Phone number
                       -> Int  -- ^ VOTO call id
                       -> Int  -- ^ VOTO tree id
                       -> Text -- ^ Schedule time
                       -> RegistrationHandler (Maybe RegistrationCall)
createRegistrationCall phone votoId votoTreeId time = 
    ulizaApiPost "registration_calls" (object call)
  where
    call = [ ("phone_number"   , String phone)
           , ("scheduled_time" , String time) 
           , ("voto_call_id"   , String (showt votoId)) 
           , ("voto_tree_id"   , String (showt votoTreeId)) ]

-- | Request the most recent 'RegistrationCall' for the provided participant 
--   from the Uliza API.
getRegistrationCall :: Participant
                    -> RegistrationHandler (Maybe RegistrationCall)
getRegistrationCall Participant{..} = join <$> sequence call
  where
    call = getRegistrationCallById <$> registrationCallId

-- | Request the 'RegistrationCall' with the given id from the Uliza API.
getRegistrationCallById :: Int -> RegistrationHandler (Maybe RegistrationCall)
getRegistrationCallById = ulizaApiGetOne "registration_calls" []

getSurveyTreeAssociation :: Int -> RegistrationHandler (Maybe SurveyTreePair)
getSurveyTreeAssociation = ulizaApiGetOne "voto_survey_registration_tree" []

-- | Schedule a registration call for a participant at the given time.
scheduleRegistrationCall :: Participant
                         -- ^ The participant to call
                         -> UTCTime
                         -- ^ Time when the registration call is to be made
                         -> Int  
                         -- ^ VOTO tree id
                         -> RegistrationHandler RegistrationCall
                         -- ^ Return the details of the scheduled call
scheduleRegistrationCall Participant{ entityId = participantId, .. }
                         time treeId = 
    -- Schedule an outgoing call with VOTO
    scheduleVotoCall phoneNumber treeId >>= maybeToEither votoErr 
    -- Post registration call to Uliza API
    >>= \CallScheduleResponse{..} -> 
          createRegistrationCall phoneNumber callId treeId (utcToText time) 
    >>= maybeToEither ulizaErr
    >>= \call -> logNoticeJSON "registration_call_scheduled" call & liftIO
     >> return call
  where
    ulizaErr = UlizaApiError "scheduleRegistrationCall: Unexpected response"
    votoErr  = UlizaApiError "scheduleRegistrationCall: Unexpected response"
    -- @TODO: VotoApiError "scheduleRegistrationCall: Unexpected response"
