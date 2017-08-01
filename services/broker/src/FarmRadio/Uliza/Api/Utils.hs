module FarmRadio.Uliza.Api.Utils where

import Data.Aeson
import Data.ByteString.Builder ( toLazyByteString )
import Data.ByteString.Lazy ( toStrict )
import Data.Scientific 
import Data.Text
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Time.Clock
import Database.PostgreSQL.Simple.Time

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

number :: Int -> Value
number = Number . fromFloatDigits . fromIntegral  

utcToText :: UTCTime -> Text
utcToText = decodeUtf8 . toStrict . toLazyByteString . utcTimeToBuilder
