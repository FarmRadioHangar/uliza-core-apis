{-# LANGUAGE TemplateHaskell #-}
module FarmRadio.Uliza.Api.Context where

import Control.Lens
import Network.Wreq ( Options )
import Network.Wreq.Session      

data ApiContext = ApiContext 
    { _baseUrl :: String
    , _options :: Options 
    , _session :: Session }
  deriving (Show)

makeLenses ''ApiContext
