{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( someFunc
    , obj
    , hoist
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
        setup
        o <- hoist (obj ^? key "data") 88
        p <- getParticipant o
        c <- join <$> sequence (getRegistrationCall <$> Participant.registrationCallId p)
        scheduleCall c
    print xx

setup :: EitherT Int (StateT RESTContext IO) ()
setup = do
    setBaseUrl "http://localhost:3000"
    setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXBpX2NvbnN1bWVyIn0.pCGD-RP8oYcYzLukq1HEKyuQ2iFMPFXpPt3Aum7aXYY"

getParticipant :: Value -> EitherT Int (StateT RESTContext IO) Participant
getParticipant payload = do
    --
    -- Extract phone number from request object
    --
    phone <- hoist (payload ^? key "subscriber_phone" . _String) 5
    --
    -- Log the raw response
    --
    post "/voto_response_data" $ object [("data", payload)] 
    --
    -- Look up participant from subscriber's phone number
    --
    response <- get $ "/participants?" <> urlEncodeVars 
                    [ ("phone_number", "eq." <> unpack phone) ]

    case firstRow response of
      Just p -> 
        --
        -- Participant exists: Done!
        -- 
        right p

      Nothing -> do
        --
        -- Create a participant if one wasn't found
        -- 
        post "/participants" $ object 
          [ ("phone_number"        , String phone)
          , ("registration_status" , "NOT_REGISTERED") ]

        right Participant { Participant.entityId           = Nothing
                          , Participant.phoneNumber        = phone
                          , Participant.registrationStatus = "NOT_REGISTERED"
                          , Participant.registrationCallId = Nothing
                          , Participant.createdAt          = "" }

getRegistrationCall :: Int -> EitherT Int (StateT RESTContext IO) (Maybe RegistrationCall)
getRegistrationCall callId = 
    firstRow <$> get ("/registration_calls?" <> urlEncodeVars [("id", "eq." <> show callId)])

scheduleCall :: Maybe RegistrationCall -> EitherT Int (StateT RESTContext IO) ()
scheduleCall call = liftIO $
    case call of
      Nothing -> print "no call" 
      Just c -> print (scheduleTime c) 

--    --what :: HttpException -> IO ()
--    --what e = 
--    --  case e of
--    --    HttpExceptionRequest req exception -> 
--    --      case exception of
--    --        StatusCodeException r bs -> print (r ^. responseStatus . statusCode)
--    --    e -> print (displayException e)

-- print (response ^. responseStatus .statusCode) & liftIO
--        print (resp ^. responseStatus . statusCode) & liftIO
