{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module FarmRadio.Uliza.Api.Client
    ( module FarmRadio.Uliza.Api.Context
    , Api
    , ApiError(..)
    , extractBool
    , extractInt
    , extractString
    , get
    , logDebug
    , logDebugJSON
    , logError
    , logErrorJSON
    , logNotice
    , logNoticeJSON
    , logWarning
    , logWarningJSON
    , loggerNamespace
    , lookupResource
    , patch
    , patchResource
    , post
    , post_
    , put
    , runApi
    , setBaseUrl
    , setHeader
    , setOauth2Token
    ) where

import Control.Lens
import Network.HTTP.Client         ( HttpExceptionContent(..)
                                   , HttpException(..) )
import Control.Monad               ( join )
import Control.Monad.IO.Class
import Control.Exception.Safe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State   ( StateT, evalStateT, modify )
import Data.Aeson 
import Data.Aeson.Lens
import Data.ByteString
import Data.Monoid                 ( (<>) )
import Data.Scientific
import Data.Text
import FarmRadio.Uliza.Api.Context
import FarmRadio.Uliza.Api.Utils   ( eitherToMaybe )
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
import System.Log.Logger

import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Lazy      as BL
import qualified Network.Wreq.Session      as Session

import qualified Data.ByteString.Lazy.Char8 

data ApiError 
  = InternalServerError       
  -- ^ Something went wrong during processing.
  | UnexpectedResponse String 
  -- ^ API response was inconsistent with the expected format.
  | StatusCodeResponse Int    
  -- ^ API server returned a non-200 response code.
  | ServerConnectionFailed    
  -- ^ Connection failed. Is the API server running?
  | AuthenticationError       
  -- ^ Unauthorized
  | NotFoundError
  -- ^ Server returned 404 NOT FOUND
  | BadRequestError           
  -- ^ Bad request format.
  deriving (Show)

type Api = EitherT ApiError (StateT ApiContext IO) 

runApi :: Api a -> IO (Either ApiError a)
runApi c = Session.withAPISession run `catch` httpException
  where
    run sess = evalStateT (runEitherT c) (ApiContext mempty opts sess) 
    opts = defaults & checkResponse .~ Just (const $ const $ return ())

httpException :: HttpException -> IO (Either ApiError a)
httpException = \case 
    HttpExceptionRequest _ (ConnectionFailure _) -> err ServerConnectionFailed
    _                                            -> err InternalServerError
  where
    err = return . Left

-- | Extract a Text key from JSON data.
extractString :: AsValue s => Text -> s -> Maybe Text
extractString k obj = obj ^? key k ._String

-- | Extract a Bool key from JSON data.
extractBool :: AsValue s => Text -> s -> Maybe Bool
extractBool k obj = 
  case obj ^? key k of
    Just (String s) -> s ^? _Bool
    Just (Bool   b) -> Just b
    _               -> Nothing

-- | Extract an Int key from JSON data.
extractInt :: AsValue s => Text -> s -> Maybe Int
extractInt k obj = case obj ^? key k of
    Just (String s) -> join (fromScientific <$> s ^? _Number)
    Just (Number n) -> fromScientific n
    _               -> Nothing
  where
    fromScientific :: Scientific -> Maybe Int
    fromScientific = eitherToMaybe . floatingOrInteger 

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
    setHeader "Prefer" ["return=representation"]
    setHeader "Accept" ["application/vnd.pgrst.object+json"]
    response <- lift $ do
      ApiContext{..} <- State.get
      Session.postWith _options _session (_baseUrl <> endpoint) body & liftIO
    decode <$> extractBody response

-- | Send a POST request.
post :: String -> Value -> Api BL.ByteString
post endpoint body = do
    liftIO $ print $ encode body
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

-- | Look up a (singleton) resource, parse the JSON response and return the 
--   result.
lookupResource :: (FromJSON a, ToJSON a) 
               => String 
               -> String 
               -> String 
               -> Api (Maybe a)
lookupResource name prop value = do
    response <- get $ resourceUrl (name <> "/" <> value) params
    return $ case decode response of
      Just [one] -> one
      _          -> Nothing
  where
    params = [ ("limit", "1") ]

-- | Send a PATCH request acting on the resource identified by the provided id.
patchResource :: String -> Int -> Value -> Api BL.ByteString
patchResource name entityId = patch $ resourceUrl name params
  where
    params = [ ("id", "eq." <> show entityId) ]

extractBody :: Response BL.ByteString -> Api BL.ByteString
extractBody response =
    case response ^. responseStatus . statusCode of
      200 -> ok
      201 -> ok                          -- 210 CREATED
      202 -> ok                          -- 202 ACCEPTED
      204 -> ok                          -- 204 NO CONTENT
      401 -> left AuthenticationError
      404 -> left NotFoundError
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

loggerNamespace :: String
loggerNamespace = "uliza_voto_registration_middleware"

logUsing :: (String -> String -> IO ()) -> String -> String -> Api ()
logUsing logger tag message = liftIO $ logger loggerNamespace 
                                     $ "[" <> tag <> "]" <> " " <> message

-- | Log a message at DEBUG priority.
logDebug :: String -> String -> Api ()
logDebug = logUsing debugM 

-- | Send a JSON object to the log at DEBUG priority.
logDebugJSON :: ToJSON a => String -> a -> Api ()
logDebugJSON tag obj = logDebug tag (jsonEncode obj)

-- | Log a message at WARNING priority.
logWarning :: String -> String -> Api ()
logWarning = logUsing warningM 

-- | Send a JSON object to the log at WARNING priority.
logWarningJSON :: ToJSON a => String -> a -> Api ()
logWarningJSON tag obj = logWarning tag (jsonEncode obj)

-- | Log a message at NOTICE priority.
logNotice :: String -> String -> Api ()
logNotice = logUsing noticeM 

-- | Send a JSON object to the log at NOTICE priority.
logNoticeJSON :: ToJSON a => String -> a -> Api ()
logNoticeJSON tag obj = logNotice tag (jsonEncode obj)

-- | Log a message at ERROR priority.
logError :: String -> String -> Api ()
logError = logUsing errorM 

-- | Send a JSON object to the log at ERROR priority.
logErrorJSON :: ToJSON a => String -> a -> Api ()
logErrorJSON tag obj = logError tag (jsonEncode obj)

jsonEncode :: ToJSON a => a -> String
jsonEncode = Data.ByteString.Lazy.Char8.unpack . encode 
