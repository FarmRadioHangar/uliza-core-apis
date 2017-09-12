module FarmRadio.Uliza.Registration.Logger
  ( logDebug
  , logWarning
  , logNotice
  , logError
  , logDebugJSON
  , logWarningJSON
  , logNoticeJSON
  , logErrorJSON
  , loggerNamespace
  ) where

import Data.Aeson
import Data.Monoid
import System.Log.Logger

import qualified Data.ByteString.Lazy.Char8

loggerNamespace :: String
loggerNamespace = "uliza_voto_registration_middleware"

logUsing :: (String -> String -> IO ()) -> String -> String -> IO ()
logUsing logger tag info = logger loggerNamespace message
  where
    message = "[" <> tag <> "]" <> " " <> info

-- | Log a message at DEBUG priority.
logDebug :: String -> String -> IO ()
logDebug = logUsing debugM

-- | Log a message at WARNING priority.
logWarning :: String -> String -> IO ()
logWarning = logUsing warningM

-- | Log a message at NOTICE priority.
logNotice :: String -> String -> IO ()
logNotice = logUsing noticeM

-- | Log a message at ERROR priority.
logError :: String -> String -> IO ()
logError = logUsing errorM

jsonEncode :: ToJSON a => a -> String
jsonEncode = Data.ByteString.Lazy.Char8.unpack . encode

-- | Log a JSON object at DEBUG priority.
logDebugJSON :: ToJSON a => String -> a -> IO ()
logDebugJSON tag obj = logDebug tag (jsonEncode obj)

-- | Log a JSON object at WARNING priority.
logWarningJSON :: ToJSON a => String -> a -> IO ()
logWarningJSON tag obj = logWarning tag (jsonEncode obj)

-- | Log a JSON object at NOTICE priority.
logNoticeJSON :: ToJSON a => String -> a -> IO ()
logNoticeJSON tag obj = logNotice tag (jsonEncode obj)

-- | Log a JSON object at ERROR priority.
logErrorJSON :: ToJSON a => String -> a -> IO ()
logErrorJSON tag obj = logError tag (jsonEncode obj)
