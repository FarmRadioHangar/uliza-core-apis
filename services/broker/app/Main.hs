{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Either.Utils           ( maybeToEither )
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Registration
import Web.Scotty (ScottyM, scotty)

import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

app :: ScottyM ()
app = 
  Scotty.post "/responses" $ do
    b <- Scotty.body 
    handler b

main :: IO ()
main = scotty 3034 app

handler :: BL.ByteString -> Scotty.ActionM ()
handler body = do
    x <- liftIO $ runApi $ do
        -- 
        setBaseUrl "http://localhost:3000"
        setOauth2Token "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXBpX2NvbnN1bWVyIn0.pCGD-RP8oYcYzLukq1HEKyuQ2iFMPFXpPt3Aum7aXYY"
        setHeader "Accept" ["*/*"]
        setHeader "Prefer" ["return=representation"]
        --
        maybeToEither BadRequestError (body ^? key "data") 
          >>= lookupParticipant 
            >>= \user -> getRegistrationCall user
              >>= determineRegistrationStatus user 
                >>= \case 
                      AlreadyRegistered         -> undefined
                      RegistrationDeclined      -> undefined
                      RegistrationCallScheduled -> undefined
                      RecentCallMade            -> undefined
                      ScheduleCall time         -> scheduleRegistrationCall user time
        --
    case x of
      Left err -> do
          liftIO $ print err
          Scotty.json $ object [("message", "error")]
      Right xyz  -> do
          liftIO $ print xyz
          Scotty.json xyz

