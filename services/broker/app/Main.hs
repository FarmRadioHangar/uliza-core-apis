{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import FarmRadio.Uliza.Api.Client
import FarmRadio.Uliza.Registration
import Web.Scotty (ScottyM, scotty)

import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scotty

app :: ScottyM ()
app = 
  Scotty.post "/responses" $ do
    Scotty.body >>= liftIO . handler
    Scotty.text "OK!"

main :: IO ()
main = scotty 3034 app

handler :: BL.ByteString -> IO ()
handler body = do
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

