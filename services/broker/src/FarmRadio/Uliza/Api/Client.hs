{-# LANGUAGE RecordWildCards #-}
module FarmRadio.Uliza.Api.Client
    ( module FarmRadio.Uliza.Api.Context
    , Api
    , ApiError(..)
    , extractString
    , get
    , getJSON
    , getSingleEntity
    , hoist
    , patch
    , patchResource
    , post
    , post_
    , put
    , runApi
    , setBaseUrl
    , setHeader
    , setOauth2Token
    , unwrapRow
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
import Data.Text
import FarmRadio.Uliza.Api.Context
import Network.HTTP.Base           ( urlEncodeVars )
import Network.Wreq                ( Options
                                   , Response
                                   , auth
                                   , customPayloadMethodWith
                                   , defaults
                                   , getWith
                                   , header
                                   , oauth2Bearer
                                   , postWith
                                   , putWith
                                   , responseBody )
import Network.HTTP.Types.Header   ( HeaderName )

import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Lazy      as BL

data ApiError 
  = ServerError 
  | AuthenticationError 
  | XXX
  deriving (Show)

type Api = EitherT ApiError (StateT ApiContext IO) 

runApi :: Api a -> IO (Either ApiError a)
runApi c = fst <$> runStateT (runEitherT c) (ApiContext mempty defaults) 

hoist :: Maybe a -> ApiError -> Api a
hoist a = hoistEither . flip maybeToEither a

extractString :: AsValue s => Text -> s -> Maybe Text
extractString k json = json ^? key k . _String

unwrapRow :: (AsValue a, ToJSON b, FromJSON b) => Response a -> Maybe b
unwrapRow response = response ^? responseBody ^._Just ._Array ^? ix 0 ._JSON 

put :: String -> Value -> Api (Response BL.ByteString)
put endpoint body = lift $ do
    ApiContext{..} <- State.get
    putWith _options (_baseUrl <> endpoint) body & liftIO

patch :: String -> Value -> Api (Response BL.ByteString)
patch endpoint body = lift $ do
    ApiContext{..} <- State.get
    customPayloadMethodWith "PATCH" _options (_baseUrl <> endpoint) body & liftIO

post_ :: (ToJSON a, FromJSON a) => String -> Value -> Api (Maybe a)
post_ endpoint body = lift $ do
    ApiContext{..} <- State.get
    unwrapRow <$> postWith _options (_baseUrl <> endpoint) body & liftIO

post :: String -> Value -> Api (Response BL.ByteString)
post endpoint body = lift $ do
    ApiContext{..} <- State.get
    postWith _options (_baseUrl <> endpoint) body & liftIO

get :: String -> Api (Response BL.ByteString)
get endpoint = lift $ do
    ApiContext{..} <- State.get
    getWith _options (_baseUrl <> endpoint) & liftIO

getJSON :: (ToJSON a, FromJSON a) => String -> Api (Maybe a)
getJSON endpoint = unwrapRow <$> get endpoint

getSingleEntity :: (FromJSON a, ToJSON a) => String -> String -> String -> Api (Maybe a)
getSingleEntity name prop value = getJSON resource
  where
    resource = "/" <> name <> "?" <> urlEncodeVars [(prop, "eq." <> value)]

patchResource :: String -> Int -> Value -> Api (Response BL.ByteString)
patchResource name entityId = patch resource 
  where
    resource = "/" <> name <> "?" <> urlEncodeVars [("id", "eq." <> show entityId)]

setBaseUrl :: String -> Api ()
setBaseUrl = lift . modify . set baseUrl 

setOauth2Token :: ByteString -> Api ()
setOauth2Token token = lift $ modify (options . auth ?~ oauth2Bearer token)

setHeader :: HeaderName -> [ByteString] -> Api ()
setHeader h values = lift $ modify (options . header h .~ values)
