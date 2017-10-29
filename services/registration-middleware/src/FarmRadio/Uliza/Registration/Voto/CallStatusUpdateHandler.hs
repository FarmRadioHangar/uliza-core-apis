{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration.Voto.CallStatusUpdateHandler
  ( votoCallStatusUpdate
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson
import Data.Either.Utils                              ( maybeToEither )
import Data.URLEncoded                                ( URLEncoded )
import FarmRadio.Uliza.Api.Utils
import FarmRadio.Uliza.Registration
import FarmRadio.Uliza.Registration.Logger
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

    complete <- isCallComplete
    print complete & liftIO

    return $ toJSON $ object []

isCallComplete :: RegistrationHandler Bool
isCallComplete = do
    state <- get
    -- Extract delivery status
    let request = state ^. params
    status <- maybeToEither BadRequestError 
                            (extractInt "delivery_status" request)
    return (6 == status)

extractInt :: String -> URLEncoded -> Maybe Int
extractInt key encoded = join (readMaybe <$> URLEncoded.lookup key encoded)

