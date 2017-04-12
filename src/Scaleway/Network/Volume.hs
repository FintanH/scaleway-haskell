
module Scaleway.Network.Volume
    ( listVolumes'
    , listVolumes
    , retrieveVolume'
    , retrieveVolume
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import           Scaleway.Types.Resource   (GetVolume, listVolume)

listVolumes' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listVolumes' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listVolume

listVolumes :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Volume])
listVolumes headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listVolume

retrieveVolume' :: HeaderToken -> Region -> GetVolume -> IO (Response ByteString)
retrieveVolume' headerToken region volume = retrieveResource' headerToken region volume

retrieveVolume :: HeaderToken -> Region -> GetVolume -> IO (Either String Get.Volume)
retrieveVolume headerToken region volume = retrieveResource headerToken region volume
