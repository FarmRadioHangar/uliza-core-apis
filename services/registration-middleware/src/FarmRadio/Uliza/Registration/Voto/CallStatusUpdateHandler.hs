{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration.Voto.CallStatusUpdateHandler
  ( votoCallStatusUpdate
  ) where

import Control.Applicative                            ( (<|>) )
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Lens
import Data.Either.Utils                              ( maybeToEither )
import Data.Maybe
import Data.Monoid
import Data.URLEncoded                                ( URLEncoded )
import FarmRadio.Uliza.Api.Participant
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Logger
import FarmRadio.Uliza.Registration.SubscriberDetails
import Text.Read                                      ( readMaybe )

import qualified Data.ByteString.Lazy.Char8           as B8
import qualified Data.URLEncoded                      as URLEncoded

-- | Response handler for VOTO call status update webhook.
votoCallStatusUpdate :: RegistrationHandler Value
votoCallStatusUpdate = do
    state <- get
    let body = state ^. requestBody
    -- Log raw VOTO webhook request object
    logDebug "incoming_call_status_update" (B8.unpack body) & liftIO
    ulizaApiPost_ "/voto_webhook_log" $ object
      [ ("data"     , String (toText body))
      , ("endpoint" , "call_status_updates") ]

    complete   <- isCallComplete
    phone      <- extract "subscriber_phone" 
    votoId     <- extract "subscriber_id"
    subscriber <- getVotoSubscriber votoId 

    print subscriber & liftIO

    let registered = join (lookup "registered" <$> properties <$> subscriber)

    case (registered, complete) of
      -- Call complete and subscriber registered
      (Just "true", True) -> 
          getOrCreateParticipant phone
            >>= registerParticipant (properties <$> subscriber)
            >>= sendResponse
      -- Call complete but 'registered' attribute not equal to 'true'
      (_, True) -> noAction "No registered attribute" "ATTR_REGISTERED_NOT_SET"
      -- Call not complete
      _ -> noAction  "Call not complete" "CALL_NOT_COMPLETE"
  where 
    noAction message reason = do
      liftIO $ logNotice "no_action" ("Participant not registered: " <> message)
      return $ toJSON $ object [("action", "NO_ACTION"), ("reason", reason)]

sendResponse = undefined

getVotoSubscriber :: Int -> RegistrationHandler (Maybe SubscriberDetails)
getVotoSubscriber sid = do
    res <- getSubscriber
    let subscr = res ^? _Just . ix "data" . ix "subscriber" 
    return $ case sequence (fromJSON <$> subscr) of
      Error _ -> Nothing
      Success a -> join (details <$> a)
  where
    getSubscriber :: RegistrationHandler (Maybe Value)
    getSubscriber = votoApiGet "/subscribers" 

extract :: (Read a) => String -> RegistrationHandler a
extract key = get >>= maybeToEither BadRequestError . \state -> do
    val <- URLEncoded.lookup key (state ^. params)
    readMaybe val <|> readMaybe (show val)

isCallComplete :: RegistrationHandler Bool
isCallComplete = (== 6) <$> extract "delivery_status"
