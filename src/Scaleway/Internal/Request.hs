
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

listResource' :: (HasResourceName resource String)
              => HeaderToken
              -> Region
              -> Page
              -> PerPage
              -> resource
              -> IO (Response ByteString)
listResource' headerToken region pageNumber nPerPage resource =
  let url = unUrl (requestUrl region) <> "/" <> (getResourceNamePlural resource)
      opts = defaults & (pageParam pageNumber) & (perPageParam nPerPage) & (scalewayHeader headerToken)
  in getWith opts url

listResource :: (FromJSON a, HasResourceName resource String)
             => HeaderToken
             -> Region
             -> Page
             -> PerPage
             -> resource
             -> IO (Either String [a])
listResource headerToken region pageNumber nPerPage resource = do
  r <- listResource' headerToken region pageNumber nPerPage resource
  return $ parseEither parseResources =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseResources = withObject resourceName $ \o -> do
      resourceList <- o .: (pack resourceName)
      traverse parseJSON resourceList

    resourceName = getResourceNamePlural resource

retrieveResource' :: (HasResourceId resource Text, HasResourceName resource String) =>
                     HeaderToken
                  -> Region
                  -> resource
                  -> IO (Response ByteString)
retrieveResource' headerToken region resource = do
  let url = unUrl (requestUrl region) <> "/" <> (getResourceNamePlural resource) <> "/" <> (unpack $ getResourceId resource)
      opts = defaults & (scalewayHeader headerToken)
  getWith opts url

retrieveResource :: (FromJSON a, HasResourceId resource Text, HasResourceName resource String) =>
                    HeaderToken
                 -> Region
                 -> resource
                 -> IO (Either String a)
retrieveResource headerToken region resource = do
  r <- retrieveResource' headerToken region resource
  return $ parseEither parseServer =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseServer = withObject resourceName $ \o -> do
      server <- o .: (pack resourceName)
      parseJSON server

    resourceName = getResourceNameSingular resource

createResource' :: ToJSON a =>
                 HeaderToken
              -> Region
              -> a
              -> Resource
              -> IO (Response ByteString)
createResource' headerToken region resouceData resource = do
  let jsonData = toJSON resouceData
      url = unUrl (requestUrl region) <> "/" <> resource
      opts = defaults & (scalewayHeader headerToken)
  print $ "POSTing: " <> show jsonData
  postWith opts url jsonData

removeResource :: HasResourceId resourceId Text => HeaderToken -> Region -> resourceId -> Resource -> IO ()
removeResource headerToken region resourceId resource = do
  let url = unUrl (requestUrl region) <> "/" <> resource <> "/" <> rId
      opts = defaults & (scalewayHeader headerToken)
  r <- deleteWith opts url
  print $ "Successfully deleted: " <> rId
  where
    rId = unpack (getResourceId resourceId)
