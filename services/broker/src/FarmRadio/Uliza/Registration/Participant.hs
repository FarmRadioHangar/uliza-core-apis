{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module FarmRadio.Uliza.Registration.Participant where

import Data.Aeson
import Data.Text

data Participant = Participant 
    { entityId           :: !(Maybe Int)
    , phoneNumber        :: !Text
    , registrationStatus :: !Text
    , registrationCallId :: !(Maybe Int)
    , createdAt          :: !Text }
  deriving (Show)

instance ToJSON Participant where
  toJSON Participant{..} = object 
    [ "id"                   .= entityId
    , "phone_number"         .= phoneNumber
    , "registration_status"  .= registrationStatus
    , "registration_call_id" .= registrationCallId
    , "created_at"           .= createdAt ]

instance FromJSON Participant where
  parseJSON = withObject "Participant" $ \v -> Participant
    <$> v .:? "id"
    <*> v .:  "phone_number"
    <*> v .:  "registration_status"
    <*> v .:? "registration_call_id"
    <*> v .:  "created_at"
