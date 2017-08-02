{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( someFunc
--    , obj
--    , hoist
--    , number
--    , extractString
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Registration
import Web.Scotty (ScottyM, scotty)

import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

-- obj = "{\"data\":{\"question_id\": \"123123\", \"survey_id\": \"89324\", \"voto_id\": \"44\", \"response_type\": \"1\", \"content_type\": \"1\", \"poll_id\": \"213\", \"delivery_log_id\": \"832\", \"choice_id\": \"1\", \"subscriber_id\": \"232\", \"subscriber_phone\": \"+233212323\", \"question_title\": \"adfadfadfasdfasfdafd\", \"choice_name\": \"adfadsfas fadsfadsfasdf\", \"date_received\": \"2017-07-24T18:13:51Z\"}}"

app :: ScottyM ()
app = Scotty.post "/registrations" $ do
    Scotty.body >>= liftIO . banan 
    Scotty.text "OK!"

someFunc :: IO ()
someFunc = scotty 3034 app

banan :: BL.ByteString -> IO ()
banan body = do
    xx <- runApi $ do
        -- 
        setBaseUrl "http://localhost:3000"
        setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXBpX2NvbnN1bWVyIn0.pCGD-RP8oYcYzLukq1HEKyuQ2iFMPFXpPt3Aum7aXYY"
        setHeader "Accept" ["*/*"]
        setHeader "Prefer" ["return=representation"]
        --
        hoist (body ^? key "data") BadRequestError
          >>= lookupParticipant 
          >>= \user -> getRegistrationCall user
          >>= \call -> scheduleCall user call
        --
    print xx



-- import Control.Exception.Safe
--    --what :: HttpException -> IO ()
--    --what e = 
--    --  case e of
--    --    HttpExceptionRequest req exception -> 
--    --      case exception of
--    --        StatusCodeException r bs -> print (r ^. responseStatus . statusCode)
--    --    e -> print (displayException e)
