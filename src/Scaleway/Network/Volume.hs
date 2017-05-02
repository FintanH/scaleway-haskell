
module Scaleway.Network.Volume
    ( listVolumes'
    , listVolumes
    , retrieveVolume'
    , retrieveVolume
    ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.ByteString.Lazy          (ByteString)
import           Network.Wreq                  (Response)
import           Scaleway.Internal.Request     (HeaderToken, Page, PerPage,
                                                listResource, listResource',
                                                retrieveResource,
                                                retrieveResource')
import           Scaleway.Internal.Types   (GetVolume, Volume, ScalewayRequest, listVolume)

listVolumes' :: (MonadReader ScalewayRequest m, MonadIO m)
             => m (Response ByteString)
listVolumes' = listResource' listVolume

listVolumes :: (MonadReader ScalewayRequest m, MonadIO m)
            => m (Either String [Volume])
listVolumes = listResource listVolume

retrieveVolume' :: (MonadReader ScalewayRequest m, MonadIO m)
                => GetVolume -> m (Response ByteString)
retrieveVolume' = retrieveResource'

retrieveVolume :: (MonadReader ScalewayRequest m, MonadIO m)
               => GetVolume -> m (Either String Volume)
retrieveVolume = retrieveResource
