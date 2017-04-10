{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Scaleway.Internal.Request where

import           Control.Lens
import           Data.Aeson              (FromJSON, Value, eitherDecode,
                                          parseJSON, withObject, (.:))
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
import           Scaleway.Types.Internal

newtype ScalewayUrl = ScalewayUrl { unUrl :: String } deriving (Show, Eq)
type HeaderToken = BS.ByteString

type Region = String
type Page = Text
type PerPage = Text

type Resource = String

requestUrl :: Region -> ScalewayUrl
requestUrl region = ScalewayUrl $  "https://cp-" <> region <> ".scaleway.com"

scalewayHeader :: HeaderToken -> Options -> Options
scalewayHeader token = \options -> options & header "X-Auth-Token" .~ [token]

pageParam :: Page -> Options -> Options
pageParam pageNumber = param "page" .~ [pageNumber]

perPageParam :: PerPage -> Options -> Options
perPageParam n = param "per_page" .~ [n]

listResource' :: HeaderToken -> Region -> Page -> PerPage -> Resource -> IO (Response ByteString)
listResource' headerToken region pageNumber nPerPage resource =
  let url = unUrl (requestUrl region) <> "/" <> resource
      opts = defaults & (pageParam pageNumber) & (perPageParam nPerPage) & (scalewayHeader headerToken)
  in getWith opts url

listResource :: (FromJSON a) => HeaderToken -> Region -> Page -> PerPage -> Resource -> IO (Either String [a])
listResource headerToken region pageNumber nPerPage resource = do
  r <- listResource' headerToken region pageNumber nPerPage resource
  return $ parseEither parseResources =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseResources = withObject resource $ \o -> do
      resourceList <- o .: (pack resource)
      traverse parseJSON resourceList

retrieveResource' :: HasResourceId r Text => HeaderToken -> Region -> Resource -> r -> IO (Response ByteString)
retrieveResource' headerToken region resource resourceId = do
  let url = unUrl (requestUrl region) <> "/" <> resource <> "/" <> (unpack $ getResourceId resourceId)
      opts = defaults & (scalewayHeader headerToken)
  getWith opts url
