module FarmRadio.Uliza.Api.Client
  ( FarmRadio.Uliza.Api.Client.get
  , FarmRadio.Uliza.Api.Client.post
  , FarmRadio.Uliza.Api.Client.patch
  ) where

import Control.Lens
import Data.Aeson
import Data.Monoid                           ( (<>) )
import FarmRadio.Uliza.Registration.Logger
import Network.Wreq
import Network.Wreq.Session
import Network.Wreq.Types

import qualified Control.Monad.Trans.State   as State
import qualified Data.ByteString.Lazy        as Lazy

type Url = String

-- | Send a POST request to the provided url and return the response.
post :: (Postable a, ToJSON a)
     => Options                        -- ^ Wreq options
     -> Session                        -- ^ Wreq session object
     -> Url                            -- ^ The url to send the request to
     -> a                              -- ^ Request body
     -> IO (Response Lazy.ByteString)  -- ^ Response as a lazy 'ByteString'
post options session url body = do
    logDebugJSON ("POST" <> " " <> url) body
    resp <- Network.Wreq.Session.postWith options session url body
    logDebug ("raw_response") (show $ resp ^. responseBody)
    return resp

-- | Send a PATCH request to the provided url and return the response.
patch :: (Postable a, ToJSON a)
      => Options                        -- ^ Wreq options
      -> Session                        -- ^ Wreq session object
      -> Url                            -- ^ The url to send the request to
      -> a                              -- ^ Request body
      -> IO (Response Lazy.ByteString)  -- ^ Response as a lazy 'ByteString'
patch options session url body = do
    logDebugJSON ("PATCH" <> " " <> url) body
    resp <- Network.Wreq.Session.customPayloadMethodWith "PATCH" options session url body
    logDebug ("raw_response") (show $ resp ^. responseBody)
    return resp

-- | Send a GET request to the provided url and return the response.
get :: Options                         -- ^ Wreq options
    -> Session                         -- ^ Wreq session object
    -> Url                             -- ^ The url to send the request to
    -> IO (Response Lazy.ByteString)   -- ^ Response as a lazy 'ByteString'
get options session url = do
    logDebug ("GET" <> " " <> url) ""
    resp <- Network.Wreq.Session.getWith options session url
    logDebug ("raw_response") (show $ resp ^. responseBody)
    return resp
