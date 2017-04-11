{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Image
    ( listImages'
    , listImages
    , retrieveImage'
    , retrieveImage
    ) where

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

listImages' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listImages' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listImage

listImages :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Image])
listImages headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listImage

retrieveImage' :: HeaderToken -> Region -> GetImage -> IO (Response ByteString)
retrieveImage' headerToken region server = retrieveResource' headerToken region server

retrieveImage :: HeaderToken -> Region -> GetImage -> IO (Either String Get.Image)
retrieveImage headerToken region server = retrieveResource headerToken region server
