{-# LANGUAGE TemplateHaskell #-}
module Api.Context where

import Network.Wreq ( Options )
import Control.Lens

data ApiContext = ApiContext 
    { _baseUrl :: String
    , _options :: Options 
    }
  deriving (Show)

makeLenses ''ApiContext
