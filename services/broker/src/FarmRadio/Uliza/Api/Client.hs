{-# LANGUAGE RecordWildCards #-}
module FarmRadio.Uliza.Api.Client
    ( module FarmRadio.Uliza.Api.Context
    , Api
    , ApiError(..)
    , extractString
    , get
    , getJSON
    , getSingleEntity
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
import Network.HTTP.Client (HttpExceptionContent(..), HttpException(..))
import Control.Monad.IO.Class
import Control.Exception.Safe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State   ( StateT, runStateT, modify )
import Data.Aeson 
import Data.Aeson.Lens
import Data.ByteString
import Data.Monoid                 ( (<>) )
import Data.Text
import FarmRadio.Uliza.Api.Context
import Network.HTTP.Base           ( urlEncodeVars )
import Network.Wreq                ( Response
                                   , auth
                                   , checkResponse
                                   , defaults
                                   , header
                                   , oauth2Bearer
                                   , responseBody 
                                   , responseStatus
                                   , statusCode )
import Network.HTTP.Types.Header   ( HeaderName )

import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Lazy      as BL
import qualified Network.Wreq.Session      as Session

data ApiError 
  = ServerError 
  | StatusCodeError Int
  | ServerConnectionError
  | AuthenticationError 
  | BadRequestError
  | XXX
  deriving (Show)

type Api = EitherT ApiError (StateT ApiContext IO) 

runApi :: Api a -> IO (Either ApiError a)
runApi c = catch (Session.withAPISession run) stuff
  where
    run sess = fst <$> runStateT 
        (runEitherT c) 
        (ApiContext mempty opts sess) 
    opts = defaults & checkResponse .~ Just (\_ _ -> return ())

stuff :: HttpException -> IO (Either ApiError a)
stuff exc = 
    case exc of
      HttpExceptionRequest _ exception -> 
        case exception of
          ConnectionFailure e -> return $ Left ServerConnectionError
          e -> do
              print e 
              return $ Left ServerError
      _ -> return $ Left ServerError

extractString :: AsValue s => Text -> s -> Maybe Text
extractString k obj = obj ^? key k . _String

unwrapRow :: (AsValue a, ToJSON b, FromJSON b) => Response a -> Maybe b
unwrapRow response = response ^? responseBody ^._Just ._Array ^? ix 0 ._JSON 

resourceUrl :: String -> [(String, String)] -> String
resourceUrl name params = "/" <> name <> "?" <> urlEncodeVars params

put :: String -> Value -> Api (Response BL.ByteString)
put endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.putWith _options _session (_baseUrl <> endpoint) body & liftIO
    createResponse response

patch :: String -> Value -> Api (Response BL.ByteString)
patch endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.customPayloadMethodWith "PATCH" _options _session 
          (_baseUrl <> endpoint) body & liftIO
    createResponse response

post_ :: (ToJSON a, FromJSON a) => String -> Value -> Api (Maybe a)
post_ endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.postWith _options _session 
          (_baseUrl <> endpoint) body & liftIO
    unwrapRow <$> createResponse response

post :: String -> Value -> Api (Response BL.ByteString)
post endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.postWith _options _session (_baseUrl <> endpoint) body & liftIO
    createResponse response

get :: String -> Api (Response BL.ByteString)
get endpoint = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.getWith _options _session (_baseUrl <> endpoint) & liftIO
    createResponse response

createResponse :: Response BL.ByteString -> Api (Response BL.ByteString)
createResponse response =
    case response ^. responseStatus . statusCode of
      200 -> ok
      201 -> ok
      202 -> ok
      401 -> left AuthenticationError
      500 -> left ServerError
      err -> left (StatusCodeError err)
  where ok = right response

getJSON :: (ToJSON a, FromJSON a) => String -> Api (Maybe a)
getJSON endpoint = unwrapRow <$> get endpoint

getSingleEntity :: (FromJSON a, ToJSON a) 
                => String 
                -> String 
                -> String 
                -> Api (Maybe a)
getSingleEntity name prop value = getJSON $ resourceUrl name params
  where
    params = [(prop, "eq." <> value)]

patchResource :: String -> Int -> Value -> Api (Response BL.ByteString)
patchResource name entityId = patch $ resourceUrl name params
  where
    params = [("id", "eq." <> show entityId)]

setBaseUrl :: String -> Api ()
setBaseUrl = lift . modify . set baseUrl 

setOauth2Token :: ByteString -> Api ()
setOauth2Token token = lift $ modify (options . auth ?~ oauth2Bearer token)

setHeader :: HeaderName -> [ByteString] -> Api ()
setHeader name val = lift $ modify (options . header name .~ val)
