{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module FarmRadio.Uliza.Registration.Voto.CallScheduleResponse where

import Data.Aeson
import Data.Text

data CallScheduleResponseData = CallScheduleResponseData
    { callId :: !Int }
  deriving (Show, Eq)

instance ToJSON CallScheduleResponseData where
  toJSON CallScheduleResponseData{..} = object 
    [ "id" .= callId ]

instance FromJSON CallScheduleResponseData where
  parseJSON = withObject "CallScheduleResponseData" $ \v -> CallScheduleResponseData
    <$> v .: "id"

data CallScheduleResponse = CallScheduleResponse
    { status :: !Int
    , _data  :: !CallScheduleResponseData }
  deriving (Show, Eq)

instance ToJSON CallScheduleResponse where
  toJSON CallScheduleResponse{..} = object 
    [ "status" .= status
    , "data"   .= _data ]

instance FromJSON CallScheduleResponse where
  parseJSON = withObject "CallScheduleResponse" $ \v -> CallScheduleResponse
    <$> v .: "status"
    <*> v .: "data"
