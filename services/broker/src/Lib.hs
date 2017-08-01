{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
    , obj
    , hoist
    , extractString
  ) where

import Api
import Control.Exception.Safe
import Control.Lens
import Control.Monad (void, when, unless, join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State   ( StateT, runStateT, modify, withState )
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import Data.Time.Format
import Database.PostgreSQL.Simple.Time
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.Wreq (Response, responseStatus, responseBody, statusCode)
import Participant (Participant(..))
import RegistrationCall (RegistrationCall(..))

import qualified Participant 
import qualified RegistrationCall

import qualified Data.ByteString.Lazy as BL

obj :: String
obj = "{\"data\":{\"question_id\": \"123123\", \"survey_id\": \"89324\", \"voto_id\": \"44\", \"response_type\": \"1\", \"content_type\": \"1\", \"poll_id\": \"213\", \"delivery_log_id\": \"832\", \"choice_id\": \"1\", \"subscriber_id\": \"232\", \"subscriber_phone\": \"+233212323\", \"question_title\": \"adfadfadfasdfasfdafd\", \"choice_name\": \"adfadsfas fadsfadsfasdf\", \"date_received\": \"2017-07-24T18:13:51Z\"}}"

someFunc :: IO ()
someFunc = do
    xx <- runApi $ do
        -- 
        setBaseUrl "http://localhost:3000"
        setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXBpX2NvbnN1bWVyIn0.pCGD-RP8oYcYzLukq1HEKyuQ2iFMPFXpPt3Aum7aXYY"
        setHeader "Accept" ["*/*"]
        setHeader "Prefer" ["return=representation"]
        --
        hoist (obj ^? key "data") 88 
          >>= lookupParticipant 
          >>= \user -> getRegistrationCall user
          >>= \call -> scheduleCall user call
        --
    print xx

getParticipantByPhoneNumber :: String -> Api (Maybe Participant)
getParticipantByPhoneNumber = getSingleEntity "participants" "phone_number"

getRegistrationCall :: Participant -> Api (Maybe RegistrationCall)
getRegistrationCall Participant{..} = 
    join <$> sequence (getRegistrationCallById <$> registrationCallId)

getRegistrationCallById :: Int -> Api (Maybe RegistrationCall)
getRegistrationCallById = getSingleEntity "registration_calls" "id" . show 

patchParticipant :: Int -> Value -> Api ()
patchParticipant p = void . patchResource "participants" p

lookupParticipant :: Value -> Api Participant
lookupParticipant request = do

    -- Extract phone number from request object
    phone <- hoist (extractString "subscriber_phone" request) 5

    -- Log the raw response
    post "/voto_response_data" $ object [("data", request)] 

    -- Look up participant from subscriber's phone number
    response <- getParticipantByPhoneNumber (unpack phone)

    let participant = [ ("phone_number"        , String phone)
                      , ("registration_status" , "NOT_REGISTERED") ]
    case response of
      --
      -- Participant exists: Done!  
      --
      Just participant -> right participant
      --
      -- Create a participant if one wasn't found
      --
      Nothing -> post_ "/participants" (object participant) >>= flip hoist 444

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

getScheduleTime :: RegistrationCall -> Maybe UTCTime
getScheduleTime RegistrationCall{..} = eitherToMaybe $ parseUTCTime (encodeUtf8 scheduleTime)

scheduleCall :: Participant -> Maybe RegistrationCall -> Api ()
scheduleCall participant@Participant{..} call = do
    --
    now <- liftIO getCurrentTime
    --
    case (join (getScheduleTime <$> call), registrationStatus) of

      (Just time, "NOT_REGISTERED") -> do

          let diff = diffUTCTime time now
          if (diff < -3000)
              then right ()
              else left 908

          --case call of
          --  Nothing -> liftIO $ print "no call" 
          --  Just r -> liftIO $ do
          --    let st = getScheduleTime r
          --    print st

      (Nothing, _) -> right ()

      (_, "REGISTERED") -> left 5

      (_, "DECLINED") -> left 6

      _ -> left 7

    let scheduleTime = addUTCTime 600 now & utcTimeToBuilder 
                                          & toLazyByteString 
                                          & toStrict 
                                          & decodeUtf8

        call = [ ("phone_number"  , String phoneNumber)
               , ("schedule_time" , String scheduleTime) ]

    response <- post_ "/registration_calls" (object call)
    userId   <- hoist participantId 111
    callId   <- hoist (join $ RegistrationCall.registrationCallId <$> response) 888 

    patchParticipant userId (toJSON participant { Participant.registrationCallId = Just callId })

    return ()


--    --what :: HttpException -> IO ()
--    --what e = 
--    --  case e of
--    --    HttpExceptionRequest req exception -> 
--    --      case exception of
--    --        StatusCodeException r bs -> print (r ^. responseStatus . statusCode)
--    --    e -> print (displayException e)

-- print (response ^. responseStatus .statusCode) & liftIO
--        print (resp ^. responseStatus . statusCode) & liftIO
