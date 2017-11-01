{-# LANGUAGE RecordWildCards #-}
module FarmRadio.Uliza.Registration.SubscriberDetails 
  ( SubscriberDetails(..)
  , details
  ) where

import Data.Aeson
import Data.Maybe                  ( catMaybes )
import Data.Text
import Text.Read                   ( readMaybe )

import qualified Data.HashMap.Lazy as Map

data SubscriberDetails = SubscriberDetails
    { votoId           :: !Int
    , receiveSms       :: !Bool
    , receiveVoice     :: !Bool
    , receiveData      :: !Bool
    , receiveUssd      :: !Bool
    , active           :: !Bool
    , isTestSubscriber :: !Bool
    , phone            :: !String
    , startDate        :: !String
    , languageId       :: !Int
    , groupIds         :: ![Int]
    , properties       :: ![(String, String)] }
  deriving (Show, Eq)

data SubscriberRawDetails = SubscriberRawDetails
    { rawVotoId           :: !String
    , rawReceiveSms       :: !String
    , rawReceiveVoice     :: !String
    , rawReceiveData      :: !String
    , rawReceiveUssd      :: !String
    , rawPhone            :: !String
    , rawActive           :: !String
    , rawStartDate        :: !String
    , rawLanguageId       :: !String
    , rawIsTestSubscriber :: !String
    , rawGroupIds         :: !String
    , rawProperties       :: !Object 
    }

instance FromJSON SubscriberRawDetails where
  parseJSON = withObject "SubscriberRawDetails" $ \v -> SubscriberRawDetails
    <$> v .: "id"
    <*> v .: "receive_sms"
    <*> v .: "receive_voice"
    <*> v .: "receive_data"
    <*> v .: "receive_ussd"
    <*> v .: "phone"
    <*> v .: "active"
    <*> v .: "start_date"
    <*> v .: "language_id"
    <*> v .: "is_test_subscriber"
    <*> v .: "group_ids"
    <*> v .: "properties"

boolDigit :: String -> Maybe Bool
boolDigit "0"     = Just False
boolDigit "false" = Just False
boolDigit "1"     = Just True
boolDigit "true"  = Just True
boolDigit _       = Nothing

extractProps :: Object -> [(String, String)]
extractProps = catMaybes . fmap f . Map.toList
  where
    f (a, String b) = Just (unpack a, unpack b)
    f _ = Nothing

details :: SubscriberRawDetails -> Maybe SubscriberDetails
details SubscriberRawDetails{..} = 
    SubscriberDetails <$> readMaybe rawVotoId
                      <*> boolDigit rawReceiveSms
                      <*> boolDigit rawReceiveVoice
                      <*> boolDigit rawReceiveData
                      <*> boolDigit rawReceiveUssd
                      <*> boolDigit rawActive
                      <*> boolDigit rawIsTestSubscriber
                      <*> pure rawPhone
                      <*> pure rawStartDate
                      <*> readMaybe rawLanguageId
                      <*> readMaybe ("[" ++ rawGroupIds ++ "]")
                      <*> pure (extractProps rawProperties)
