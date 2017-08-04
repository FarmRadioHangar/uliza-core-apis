{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration.RegistrationCall where

import Data.Aeson
import Data.Text

data RegistrationCall = RegistrationCall 
    { entityId           :: !(Maybe Int)
    , phoneNumber        :: !Text
    , scheduledTime      :: !Text 
    , createdAt          :: !Text }
  deriving (Show, Eq)

instance ToJSON RegistrationCall where
  toJSON RegistrationCall{..} = object 
    [ "id"             .= entityId
    , "phone_number"   .= phoneNumber
    , "scheduled_time" .= scheduledTime
    , "created_at"     .= createdAt ]

instance FromJSON RegistrationCall where
  parseJSON = withObject "RegistrationCall" $ \v -> RegistrationCall
    <$> v .:? "id"
    <*> v .:  "phone_number"
    <*> v .:  "scheduled_time"
    <*> v .:  "created_at"
