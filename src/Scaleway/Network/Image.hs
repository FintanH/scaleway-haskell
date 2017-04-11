{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Image where

import           Control.Lens
import           Data.Aeson                (Value, eitherDecode, withObject,
                                            (.:))
import           Data.Aeson.Types          (parseEither, parseJSON, toJSON)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, unpack)
import           Network.Wreq              (Response, defaults, deleteWith,
                                            getWith, postWith, responseBody)
import           Scaleway.Internal.Request
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal
import qualified Scaleway.Types.Post       as Post
import           Scaleway.Types.Resource   (GetImage, listImage)

listServers' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listServers' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listImage

listServers :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Image])
listServers headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listImage

retrieveServer' :: HeaderToken -> Region -> GetImage -> IO (Response ByteString)
retrieveServer' headerToken region server = retrieveResource' headerToken region server

retrieveServer :: HeaderToken -> Region -> GetImage -> IO (Either String Get.Image)
retrieveServer headerToken region server = retrieveResource headerToken region server
