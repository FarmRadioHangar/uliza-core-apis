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
