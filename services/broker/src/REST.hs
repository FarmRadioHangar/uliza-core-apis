{-# LANGUAGE RecordWildCards #-}
module REST 
    ( module REST.Context
    , RESTClient
    , runREST
    , hoist
    , firstRow
    , setBaseUrl
    , setOauth2Token
    , get
    , post
    ) where

import Control.Lens
import Control.Monad               ( void )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State   ( StateT, runStateT, modify, withState )
import Data.Aeson 
import Data.Aeson.Lens
import Data.ByteString
import Data.Either.Utils           ( maybeToEither )
import Data.Monoid                 ( (<>) )
import Network.Wreq                ( Options, Response, auth, defaults, oauth2Bearer, getWith, postWith, responseBody )
import REST.Context

import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Lazy      as BL

type RESTClient = EitherT Int (StateT RESTContext IO) 

runREST :: RESTClient a -> IO (Either Int a)
runREST c = fst <$> runStateT (runEitherT c) (RESTContext mempty defaults) 

hoist :: Maybe a -> Int -> RESTClient a
hoist a = hoistEither . flip maybeToEither a

firstRow :: (AsValue a, ToJSON b, FromJSON b) => Response a -> Maybe b
firstRow response = response ^? responseBody ^._Just ._Array ^? ix 0 ._JSON 

post :: String -> Value -> RESTClient (Response BL.ByteString)
post endpoint body = lift $ do
  RESTContext{..} <- State.get
  postWith _options (_baseUrl <> endpoint) body & liftIO

get :: String -> RESTClient (Response BL.ByteString)
get endpoint = lift $ do
  RESTContext{..} <- State.get
  getWith _options (_baseUrl <> endpoint) & liftIO

setBaseUrl :: String -> RESTClient ()
setBaseUrl = lift . modify . set baseUrl 

setOauth2Token :: ByteString -> RESTClient ()
setOauth2Token token = lift $ modify (options . auth ?~ oauth2Bearer token)
