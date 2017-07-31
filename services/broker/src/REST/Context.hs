{-# LANGUAGE TemplateHaskell #-}
module REST.Context where

import Network.Wreq ( Options )
import Control.Lens

data RESTContext = RESTContext 
    { _baseUrl :: String
    , _options :: Options 
    }
  deriving (Show)

makeLenses ''RESTContext
