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
import System.Log.Logger hiding ( Logger )

import qualified Data.ByteString.Lazy.Char8

loggerNamespace :: String
loggerNamespace = "uliza_voto_registration_middleware"

type Logger = String -> String -> IO ()

logUsing :: Logger -> Logger
logUsing logger tag info = logger loggerNamespace message
  where
    message = "[" <> tag <> "]" <> " " <> info

-- | Log a message at DEBUG priority.
logDebug :: Logger
logDebug = logUsing debugM

-- | Log a message at WARNING priority.
logWarning :: Logger
logWarning = logUsing warningM

-- | Log a message at NOTICE priority.
logNotice :: Logger
logNotice = logUsing noticeM

-- | Log a message at ERROR priority.
logError :: Logger
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
