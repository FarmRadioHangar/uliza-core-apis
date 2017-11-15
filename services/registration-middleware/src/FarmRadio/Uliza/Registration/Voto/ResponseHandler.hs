{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FarmRadio.Uliza.Registration.Voto.ResponseHandler
  ( votoResponse
  ) where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Either.Utils                              ( maybeToEither )
import Data.Monoid
import Data.Text
import FarmRadio.Uliza.Api.Participant
import FarmRadio.Uliza.Api.RegistrationCall
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Logger
import FarmRadio.Uliza.Registration.SurveyTreePair

import qualified Data.ByteString.Lazy.Char8           as B8

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
    phone  <- extract "subscriber_phone"
    survey <- extract "survey_id" 
    -- If the phone number is not already associated with a participant in the
    -- database, one is created here
    participant <- getOrCreateParticipant phone
    -- Find most recent registration call (if one exists) for this participant
    call <- getRegistrationCall participant
    -- Determine the participant's registration status
    status <- determineRegistrationStatus participant call
    -- Get the registration tree associated with this survey (if one exists)
    tree <- getSurveyTreeAssociation survey
    -- Take some action, then log and respond
    case (tree, status) of
      ( _, AlreadyRegistered )    -> noop "ALREADY_REGISTERED"
      ( _, RegistrationDeclined ) -> noop "REGISTRATION_DECLINED"
      ( _, PriorCallScheduled )   -> noop "PRIOR_CALL_SCHEDULED"
      ( _, RecentCallMade )       -> noop "TOO_SOON"
      ( Nothing, _ )              -> noop "NO_REGISTRATION_TREE"
      ( Just SurveyTreePair{..}, ScheduleCall time ) -> do

        liftIO $ print "-------------------------------------"
        liftIO $ print tree
        liftIO $ print "-------------------------------------"

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
