
module Scaleway.Network.Image
    ( listImages'
    , listImages
    , retrieveImage'
    , retrieveImage
    ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import           Scaleway.Internal.ScalewayEnv (ScalewayEnv)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetImage, listImage)

listImages' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listImages' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listImage

listImages :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Image])
listImages headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listImage

retrieveImage' :: (MonadReader ScalewayEnv m, MonadIO m)
               => GetImage -> m (Response ByteString)
retrieveImage' = retrieveResource'

retrieveImage :: (MonadReader ScalewayEnv m, MonadIO m)
              => GetImage -> m (Either String Get.Image)
retrieveImage = retrieveResource
