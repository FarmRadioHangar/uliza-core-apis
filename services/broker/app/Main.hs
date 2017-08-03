{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Either.Utils                ( maybeToEither )
import Data.Text                        ( Text )
import Data.Text.Format                 ( Only(..), format )
import Data.Text.Lazy                   ( toStrict )
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Registration
import Network.HTTP.Types
import Web.Scotty                       ( ScottyM, scotty )

import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

main :: IO ()
main = scotty 3034 app

app :: ScottyM ()
app = Scotty.post "/responses" (Scotty.body >>= handler)

handler :: BL.ByteString -> Scotty.ActionM ()
handler body = either errorResponse Scotty.json =<< liftIO (runApi task)
  where
    task = do
      -- 
      setBaseUrl "http://localhost:3000"
      setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXBpX2NvbnN1bWVyIn0.pCGD-RP8oYcYzLukq1HEKyuQ2iFMPFXpPt3Aum7aXYY"
      setHeader "Accept" ["application/json"]
      setHeader "User-Agent" ["Uliza VOTO Registration Middleware"]
      --
      maybeToEither BadRequestError (body ^? key "data") 
        >>= lookupParticipant 
          >>= \user -> getRegistrationCall user
            >>= determineRegistrationStatus user 
              >>= \case 
                AlreadyRegistered    -> notScheduled "PARTICIPANT_PREVIOUSLY_REGISTERED"
                RegistrationDeclined -> notScheduled "REGISTRATION_DECLINED"
                PriorCallScheduled   -> notScheduled "PRIOR_CALL_SCHEDULED"
                RecentCallMade       -> notScheduled "RECENT_CALL"
                ScheduleCall time    -> toJSON <$> scheduleRegistrationCall user time

    notScheduled message = return $ object [("message", String message)]
    errorResponse err = do
        liftIO $ print err
        Scotty.status internalServerError500
        Scotty.json $ object [("error", String (hint err))]

    hint :: ApiError -> Text
    hint InternalServerError       = "INTERNAL_SERVER_ERROR"
    hint UnexpectedResponse        = "UNEXPECTED_RESPONSE_FORMAT"
    hint (StatusCodeResponse code) = statusCodeResponse code "STATUS_CODE_{}" 
    hint ServerConnectionError     = "SERVER_CONNECTION_ERROR"
    hint AuthenticationError       = "UNAUTHORIZED"
    hint BadRequestError           = "BAD_REQUEST_FORMAT"

    statusCodeResponse code = toStrict . flip format (Only code) 

