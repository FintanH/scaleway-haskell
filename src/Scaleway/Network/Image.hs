
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
import           Scaleway.Internal.Types   (Region, Image, ScalewayEnv, GetImage, listImage)

listImages' :: (MonadReader ScalewayEnv m, MonadIO m)
            => Page -> PerPage -> m (Response ByteString)
listImages' pageNumber nPerPage = listResource' pageNumber nPerPage listImage

listImages :: (MonadReader ScalewayEnv m, MonadIO m)
           => Page -> PerPage -> m (Either String [Image])
listImages pageNumber nPerPage = listResource pageNumber nPerPage listImage

retrieveImage' :: (MonadReader ScalewayEnv m, MonadIO m)
               => GetImage -> m (Response ByteString)
retrieveImage' = retrieveResource'

retrieveImage :: (MonadReader ScalewayEnv m, MonadIO m)
              => GetImage -> m (Either String Image)
retrieveImage = retrieveResource
