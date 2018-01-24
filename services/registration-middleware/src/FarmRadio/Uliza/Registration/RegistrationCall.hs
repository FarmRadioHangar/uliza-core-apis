{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration.RegistrationCall where

import Data.Aeson
import Data.Text

data RegistrationCall = RegistrationCall 
    { entityId           :: !(Maybe Int)
    , phoneNumber        :: !Text
    , scheduledTime      :: !Text 
    , votoCallId         :: !Int
    , votoTreeId         :: !Int
    , createdAt          :: !Text }
  deriving (Show, Eq)

instance ToJSON RegistrationCall where
  toJSON RegistrationCall{..} = object 
    [ "id"             .= entityId
    , "phone_number"   .= phoneNumber
    , "scheduled_time" .= scheduledTime
    , "voto_call_id"   .= votoCallId
    , "voto_tree_id"   .= votoTreeId
    , "created_at"     .= createdAt ]

instance FromJSON RegistrationCall where
  parseJSON = withObject "RegistrationCall" $ \v -> RegistrationCall
    <$> v .:? "id"
    <*> v .:  "phone_number"
    <*> v .:  "scheduled_time"
    <*> v .:  "voto_call_id"
    <*> v .:  "voto_tree_id"
    <*> v .:  "created_at"
