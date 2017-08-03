{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module FarmRadio.Uliza.Api.Client
    ( module FarmRadio.Uliza.Api.Context
    , Api
    , ApiError(..)
    , extractString
    , get
    , getResource
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
  = InternalServerError 
  | UnexpectedResponse
  | StatusCodeResponse Int
  | ServerConnectionError
  | AuthenticationError 
  | BadRequestError
  deriving (Show)

type Api = EitherT ApiError (StateT ApiContext IO) 

runApi :: Api a -> IO (Either ApiError a)
runApi c = catch (Session.withAPISession run) httpException
  where
    run sess = fst <$> runStateT 
        (runEitherT c) 
        (ApiContext mempty opts sess) 
    opts = defaults & checkResponse .~ Just (\_ _ -> return ())

httpException :: HttpException -> IO (Either ApiError a)
httpException = 
  \case 
    HttpExceptionRequest _ (ConnectionFailure _) -> err ServerConnectionError
    _                                            -> err InternalServerError
  where
    err = return . Left

extractString :: AsValue s => Text -> s -> Maybe Text
extractString k obj = obj ^? key k . _String

unwrapRow :: (ToJSON b, FromJSON b) => BL.ByteString -> Maybe b
unwrapRow body = body ^?_Array . ix 0 ._JSON 

resourceUrl :: String -> [(String, String)] -> String
resourceUrl name params = "/" <> name <> "?" <> urlEncodeVars params

-- | Send a PUT request.
put :: String -> Value -> Api BL.ByteString
put endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.putWith _options _session (_baseUrl <> endpoint) body & liftIO
    extractBody response

-- | Send a PATCH request.
patch :: String -> Value -> Api BL.ByteString
patch endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.customPayloadMethodWith "PATCH" _options _session 
          (_baseUrl <> endpoint) body & liftIO
    extractBody response

-- | Send a POST request, and subsequently capture, parse, and return the JSON 
--   response.
post_ :: (ToJSON a, FromJSON a) => String -> Value -> Api (Maybe a)
post_ endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.postWith _options _session 
          (_baseUrl <> endpoint) body & liftIO
    unwrapRow <$> extractBody response

-- | Send a POST request.
post :: String -> Value -> Api BL.ByteString
post endpoint body = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.postWith _options _session (_baseUrl <> endpoint) body & liftIO
    extractBody response

-- | Send a GET request.
get :: String -> Api BL.ByteString
get endpoint = do
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.getWith _options _session (_baseUrl <> endpoint) & liftIO
    extractBody response

-- | Send a GET request, parse the JSON response and return the first row of 
--   the result.
getResource :: (FromJSON a, ToJSON a) 
            => String 
            -> String 
            -> String 
            -> Api (Maybe a)
getResource name prop value = unwrapRow <$> get (resourceUrl name params)
  where
    params = [(prop, "eq." <> value)]

patchResource :: String -> Int -> Value -> Api BL.ByteString
patchResource name entityId = patch $ resourceUrl name params
  where
    params = [("id", "eq." <> show entityId)]

extractBody :: Response BL.ByteString -> Api BL.ByteString
extractBody response =
    case response ^. responseStatus . statusCode of
      200 -> ok
      201 -> ok
      202 -> ok
      401 -> left AuthenticationError
      500 -> left InternalServerError
      err -> left (StatusCodeResponse err)
  where 
    ok = right (response ^. responseBody)

-- | Specify the API base url.
setBaseUrl :: String -> Api ()
setBaseUrl = lift . modify . set baseUrl 

-- | Set an OAuth2.0 token (JWT) for authentication with API server.
setOauth2Token :: ByteString -> Api ()
setOauth2Token token = lift $ modify (options . auth ?~ oauth2Bearer token)

setHeader :: HeaderName -> [ByteString] -> Api ()
setHeader name val = lift $ modify (options . header name .~ val)
