module FarmRadio.Uliza.Api.Utils
  ( toText
  , eitherToMaybe
  , utcToText
  ) where

import Data.ByteString.Builder          ( toLazyByteString )
import Data.ByteString.Lazy             ( toStrict )
import Data.Text
import Data.Text.Encoding               ( decodeUtf8 )
import Data.Time.Clock
import Database.PostgreSQL.Simple.Time

import qualified Data.ByteString.Lazy as Lazy

toText :: Lazy.ByteString -> Text
toText = decodeUtf8 . Data.ByteString.Lazy.toStrict

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

utcToText :: UTCTime -> Text
utcToText = decodeUtf8 . toStrict . toLazyByteString . utcTimeToBuilder
