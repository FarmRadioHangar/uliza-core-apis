module FarmRadio.Uliza.Api.Utils
  ( toText
  , eitherToMaybe
  , utcToText
--  , extract
--  , extractString
  ) where

import Control.Monad
import Data.ByteString.Builder          ( toLazyByteString )
import Data.ByteString.Lazy             ( toStrict )
import Data.Text
import Data.Text.Encoding               ( decodeUtf8 )
import Data.Time.Clock
--import Data.URLEncoded                  ( URLEncoded )
import Database.PostgreSQL.Simple.Time
--import Text.Read                        ( readMaybe )

import qualified Data.ByteString.Lazy as Lazy
--import qualified Data.URLEncoded      as URLEncoded

toText :: Lazy.ByteString -> Text
toText = decodeUtf8 . Data.ByteString.Lazy.toStrict

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

utcToText :: UTCTime -> Text
utcToText = decodeUtf8 . toStrict . toLazyByteString . utcTimeToBuilder

--extract :: Read a => String -> URLEncoded -> Maybe a
--extract key encoded = join (readMaybe <$> URLEncoded.lookup key encoded)
--
--extractString :: String -> URLEncoded -> Maybe String
--extractString = URLEncoded.lookup 
