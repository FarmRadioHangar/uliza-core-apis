{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
    , obj
    , hoist
    , extractString
  ) where

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
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (encodeUtf8, utcTimeToBuilder)
import Data.Time.Clock
import Data.Time.Format
import Database.PostgreSQL.Simple.Time
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.Wreq (Response, responseStatus, responseBody, statusCode)
import Participant (Participant(..))
import REST
import RegistrationCall (RegistrationCall(..))

import qualified Participant 
import qualified RegistrationCall

obj :: String
obj = "{\"data\":{\"question_id\": \"123123\", \"survey_id\": \"89324\", \"voto_id\": \"44\", \"response_type\": \"1\", \"content_type\": \"1\", \"poll_id\": \"213\", \"delivery_log_id\": \"832\", \"choice_id\": \"1\", \"subscriber_id\": \"232\", \"subscriber_phone\": \"+233212323\", \"question_title\": \"adfadfadfasdfasfdafd\", \"choice_name\": \"adfadsfas fadsfadsfasdf\", \"date_received\": \"2017-07-24T18:13:51Z\"}}"

someFunc :: IO ()
someFunc = do
    xx <- runREST $ do
        -- 
        setBaseUrl "http://localhost:3000"
        setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXBpX2NvbnN1bWVyIn0.pCGD-RP8oYcYzLukq1HEKyuQ2iFMPFXpPt3Aum7aXYY"
        --
        hoist (obj ^? key "data") 88 
        >>= getParticipant 
        >>= \user -> getRegistrationCall user
        >>= \call -> scheduleCall user call
        --
    print xx

extractString :: AsValue s => Text -> s -> Maybe Text
extractString k json = json ^? key k . _String

getParticipant :: Value -> RESTClient Participant
getParticipant request = do
    --
    -- Extract phone number from request object
    --
    phone <- hoist (extractString "subscriber_phone" request) 5
    --
    -- Log the raw response
    --
    post "/voto_response_data" $ object [("data", request)] 
    --
    -- Look up participant from subscriber's phone number
    --
    response <- get $ "/participants?" <> urlEncodeVars [("phone_number", "eq." <> unpack phone)]
    --
    case firstRow response of
      Just participant -> 
        --
        -- Participant exists: Done!  
        --
        right participant
        -- 
      Nothing -> do
        --
        -- Create a participant if one wasn't found
        -- 
        post "/participants" $ object 
          [ ("phone_number"        , String phone)
          , ("registration_status" , "NOT_REGISTERED") ]
        --
        right Participant { Participant.entityId           = Nothing
                          , Participant.phoneNumber        = phone
                          , Participant.registrationStatus = "NOT_REGISTERED"
                          , Participant.registrationCallId = Nothing
                          , Participant.createdAt          = "" }

getRegistrationCall :: Participant -> RESTClient (Maybe RegistrationCall)
getRegistrationCall p = join <$> sequence (xxx <$> Participant.registrationCallId p)
  where
    xxx callId = firstRow <$> get ("/registration_calls?" <> urlEncodeVars [("id", "eq." <> show callId)])

scheduleCall :: Participant -> Maybe RegistrationCall -> RESTClient ()
scheduleCall Participant{..} call = do
    --
    now <- liftIO $ getCurrentTime
    --
    case registrationStatus of
      "NOT_REGISTERED" -> 
        case call of
          Nothing -> liftIO $ print "no call" 
          Just RegistrationCall{..} -> liftIO $ do
            let st = parseUTCTime (encodeUtf8 scheduleTime)
            print st
      --
      "REGISTERED" -> left 5
      --
      "DECLINED" -> left 6
      --
      _ -> left 7


    let time = addUTCTime 600 now

    -- let bldr = utcTimeToBuilder now

    void $ post "/registration_calls" $ object 
      [ ("phone_number"  , String $ phoneNumber)
      , ("schedule_time" , String $ pack $ show time) ]



--    --what :: HttpException -> IO ()
--    --what e = 
--    --  case e of
--    --    HttpExceptionRequest req exception -> 
--    --      case exception of
--    --        StatusCodeException r bs -> print (r ^. responseStatus . statusCode)
--    --    e -> print (displayException e)

-- print (response ^. responseStatus .statusCode) & liftIO
--        print (resp ^. responseStatus . statusCode) & liftIO
