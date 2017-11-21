{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module FarmRadio.Uliza.Registration.Voto.CallScheduleResponse where

import Data.Aeson
import Data.Text

data CallScheduleResponse = CallScheduleResponse
    { status :: !Int
    , callId :: !Int }
  deriving (Show, Eq)

instance ToJSON CallScheduleResponse where
  toJSON CallScheduleResponse{..} = object 
    [ "status" .= status
    , "data"   .= callId ]

instance FromJSON CallScheduleResponse where
  parseJSON = withObject "CallScheduleResponse" $ \v -> CallScheduleResponse
    <$> v .: "status"
    <*> v .: "data"
