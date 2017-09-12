module FarmRadio.Uliza.Api.Client
  ( FarmRadio.Uliza.Api.Client.get
  , FarmRadio.Uliza.Api.Client.post
  ) where

import Control.Lens
import Data.Aeson
import Data.Monoid                           ( (<>) )
import FarmRadio.Uliza.Registration.Logger
import Network.Wreq
import Network.Wreq.Session
import Network.Wreq.Types

import qualified Control.Monad.Trans.State   as State
import qualified Data.ByteString.Lazy        as BL

type Url = String

-- | Send a POST request to the provided url and return the response.
post :: (Postable a, ToJSON a)
     => Options                        -- ^ Wreq options
     -> Session                        -- ^ Wreq session object
     -> Url                            -- ^ The url to send the request to
     -> a                              -- ^ Request body
     -> IO (Response BL.ByteString)    -- ^ Response as a lazy 'ByteString'
post options session url body = do
    logDebugJSON ("POST" <> " " <> url) body
    Network.Wreq.Session.postWith options session url body

-- | Send a GET request to the provided url and return the response.
get :: Options                         -- ^ Wreq options
    -> Session                         -- ^ Wreq session object
    -> Url                             -- ^ The url to send the request to
    -> IO (Response BL.ByteString)     -- ^ Response as a lazy 'ByteString'
get options session url = do
    logDebug ("GET" <> " " <> url) ""
    Network.Wreq.Session.getWith options session url
