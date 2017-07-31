{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module RegistrationCall where

import Data.Aeson
import Data.Text

data RegistrationCall = RegistrationCall 
    { entityId           :: !(Maybe Int)
    , phoneNumber        :: !Text
    , scheduleTime       :: !Text 
    , createdAt          :: !Text }
  deriving (Show)

instance ToJSON RegistrationCall where
  toJSON RegistrationCall{..} = object 
    [ "id"            .= entityId
    , "phone_number"  .= phoneNumber
    , "schedule_time" .= scheduleTime
    , "created_at"    .= createdAt ]

instance FromJSON RegistrationCall where
  parseJSON = withObject "RegistrationCall" $ \v -> RegistrationCall
    <$> v .:? "id"
    <*> v .:  "phone_number"
    <*> v .:  "schedule_time"
    <*> v .:  "created_at"
