{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration.Voto.ResponseHandler
  ( votoResponse
  ) where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Either.Utils                              ( maybeToEither )
import Data.Monoid
import Data.Text
import Data.URLEncoded                                ( URLEncoded )
import FarmRadio.Uliza.Api.Participant
import FarmRadio.Uliza.Api.RegistrationCall
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Logger
import FarmRadio.Uliza.Registration.SurveyTreePair
import Text.Read                                      ( readMaybe )

import qualified Data.URLEncoded                      as URLEncoded
import qualified Data.ByteString.Lazy.Char8           as B8

getTreeId :: Int -> RegistrationHandler (Maybe Int)
getTreeId survey = do
    state <- get
    case URLEncoded.lookup ("tree_id" :: String) (state ^. params) of
      Just t -> return (readMaybe t)
      Nothing -> do
        tree <- getSurveyTreeAssociation survey
        return (votoTreeId <$> tree)

-- | Response handler for survey response webhook -- triggered by VOTO every
--   time a survey response is recorded.
votoResponse :: RegistrationHandler Value
votoResponse = do
    state <- get
    let body = state ^. requestBody
    -- Log raw VOTO webhook request object
    logDebug "incoming_response" (B8.unpack body) & liftIO
    ulizaApiPost_ "voto_webhook_log" $ object
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
    -- Find the tree id for this survey, from the request params or (if absent)
    -- from Uliza
    treeId <- getTreeId survey
    -- Take some action, then log and respond
    case (treeId, status) of
      ( _, AlreadyRegistered )    -> noop "ALREADY_REGISTERED"
      ( _, RegistrationDeclined ) -> noop "REGISTRATION_DECLINED"
      ( _, PriorCallScheduled )   -> noop "PRIOR_CALL_SCHEDULED"
      ( _, RecentCallMade )       -> noop "TOO_SOON"
      ( Nothing, _ )              -> noop "NO_REGISTRATION_TREE"
      ( Just tree, ScheduleCall time ) -> do
        logDebug "survery_registration_tree" (show tree) & liftIO
        call <- scheduleRegistrationCall participant time tree
        return $ toJSON $ object
          [ ("action", "REGISTRATION_CALL_SCHEDULED")
          , ("registration_call", toJSON call) ]
  where
    -- Request successfully processed, but no registration call was scheduled
    noop :: Text -> RegistrationHandler Value
    noop reason = do
      liftIO $ logNotice "no_call_scheduled"
        ("A registration call was NOT scheduled. Reason: " <> unpack reason)
      return $ object $ fmap String <$> [ ("reason" , reason)
                                        , ("action" , "NONE") ]
