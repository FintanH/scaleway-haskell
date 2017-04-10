{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Volumes where

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
import           Scaleway.Types.Resource   (GET, ResourceType (VolumeResource))

listVolumes' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listVolumes' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage "volumes"

listVolumes :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Volume])
listVolumes headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage "volumes"

retrieveVolume' :: HeaderToken -> Region -> GET VolumeResource -> IO (Response ByteString)
retrieveVolume' headerToken region volume = retrieveResource' headerToken region volume

retrieveVolume :: HeaderToken -> Region -> GET VolumeResource -> IO (Either String Get.Volume)
retrieveVolume headerToken region volume = retrieveResource headerToken region volume
