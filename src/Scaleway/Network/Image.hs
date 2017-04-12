
module Scaleway.Network.Image
    ( listImages'
    , listImages
    , retrieveImage'
    , retrieveImage
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetImage, listImage)

listImages' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listImages' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listImage

listImages :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Image])
listImages headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listImage

retrieveImage' :: HeaderToken -> Region -> GetImage -> IO (Response ByteString)
retrieveImage' headerToken region server = retrieveResource' headerToken region server

retrieveImage :: HeaderToken -> Region -> GetImage -> IO (Either String Get.Image)
retrieveImage headerToken region server = retrieveResource headerToken region server
