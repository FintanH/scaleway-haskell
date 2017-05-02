
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
import           Scaleway.Internal.Types   (Region, Image, ScalewayRequest, GetImage, listImage)

listImages' :: (MonadReader ScalewayRequest m, MonadIO m)
            => m (Response ByteString)
listImages' = listResource' listImage

listImages :: (MonadReader ScalewayRequest m, MonadIO m)
           => m (Either String [Image])
listImages = listResource listImage

retrieveImage' :: (MonadReader ScalewayRequest m, MonadIO m)
               => GetImage -> m (Response ByteString)
retrieveImage' = retrieveResource'

retrieveImage :: (MonadReader ScalewayRequest m, MonadIO m)
              => GetImage -> m (Either String Image)
retrieveImage = retrieveResource
