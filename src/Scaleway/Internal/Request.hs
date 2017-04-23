
{-# LANGUAGE FlexibleContexts #-}

module Scaleway.Internal.Request where

import           Control.Lens
import           Data.Aeson              (FromJSON, ToJSON, Value, eitherDecode,
                                          parseJSON, toJSON, withObject, (.:))
import           Data.Aeson.Types        (parseEither)
import qualified Data.ByteString         as BS
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Data.Text               (pack, unpack)
import           Network.Wreq            (Options, Response, defaults,
                                          deleteWith, getWith, header, param,
                                          postWith, responseBody)
import           Scaleway.Types.Internal hiding (HasResourceId)
import Scaleway.Types.Resource (HasResourceId(..), HasResourceName(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Scaleway.Internal.ScalewayEnv (ScalewayEnv (..))

newtype ScalewayUrl = ScalewayUrl { unUrl :: String } deriving (Show, Eq)
type HeaderToken = BS.ByteString

type Page = Text
type PerPage = Text

type Resource = String

requestUrl :: Region -> ScalewayUrl
requestUrl region = ScalewayUrl $  "https://cp-" <> show region <> ".scaleway.com"

scalewayHeader :: HeaderToken -> Options -> Options
scalewayHeader token = \options -> options & header "X-Auth-Token" .~ [token]

pageParam :: Page -> Options -> Options
pageParam pageNumber = param "page" .~ [pageNumber]

perPageParam :: PerPage -> Options -> Options
perPageParam n = param "per_page" .~ [n]

listResource' :: (HasResourceName resource String,
                  MonadReader ScalewayEnv m, MonadIO m)
              => Page
              -> PerPage
              -> resource
              -> m (Response ByteString)
listResource' pageNumber nPerPage resource = do
  scalewayEnv <- ask
  let url = unUrl (requestUrl $ region scalewayEnv) <> "/" <> (getResourceNamePlural resource)
      opts = defaults & (pageParam pageNumber) & (perPageParam nPerPage) & (scalewayHeader $ authToken scalewayEnv)
  liftIO (getWith opts url)

listResource :: (FromJSON a, HasResourceName resource String,
                 MonadReader ScalewayEnv m, MonadIO m)
             => Page
             -> PerPage
             -> resource
             -> m (Either String [a])
listResource pageNumber nPerPage resource = do
  r <- listResource' pageNumber nPerPage resource
  return $ parseEither parseResources =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseResources = withObject resourceName $ \o -> do
      resourceList <- o .: (pack resourceName)
      traverse parseJSON resourceList

    resourceName = getResourceNamePlural resource

retrieveResource' :: (HasResourceId resource Text, HasResourceName resource String,
                      MonadReader ScalewayEnv m, MonadIO m)
                  => resource
                  -> m (Response ByteString)
retrieveResource' resource = do
  scalewayEnv <- ask
  let url = unUrl (requestUrl $ region scalewayEnv) <> "/" <> (getResourceNamePlural resource) <> "/" <> (unpack $ getResourceId resource)
      opts = defaults & (scalewayHeader $ authToken scalewayEnv)
  liftIO (getWith opts url)

retrieveResource :: (FromJSON a, HasResourceId resource Text, HasResourceName resource String,
                     MonadReader ScalewayEnv m, MonadIO m)
                 => resource
                 -> m (Either String a)
retrieveResource resource = do
  r <- retrieveResource' resource
  return $ parseEither parseServer =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseServer = withObject resourceName $ \o -> do
      server <- o .: (pack resourceName)
      parseJSON server

    resourceName = getResourceNameSingular resource

createResource' :: (ToJSON a, MonadReader ScalewayEnv m, MonadIO m)
                => a
                -> Resource
                -> m (Response ByteString)
createResource' resouceData resource = do
  scalewayEnv <- ask
  let jsonData = toJSON resouceData
      url = unUrl (requestUrl $ region scalewayEnv) <> "/" <> resource
      opts = defaults & (scalewayHeader $ authToken scalewayEnv)
  liftIO (print $ "POSTing: " <> show jsonData)
  liftIO (postWith opts url jsonData)

removeResource :: (HasResourceId resourceId Text, MonadReader ScalewayEnv m, MonadIO m)
               => resourceId -> Resource -> m ()
removeResource resourceId resource = do
  scalewayEnv <- ask
  let url = unUrl (requestUrl $ region scalewayEnv) <> "/" <> resource <> "/" <> rId
      opts = defaults & (scalewayHeader $ authToken scalewayEnv)
  r <- liftIO $ deleteWith opts url
  liftIO (print $ "Successfully deleted: " <> rId)
  where
    rId = unpack (getResourceId resourceId)
