
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
import           Scaleway.Internal.Types   (GetVolume, Volume, ScalewayEnv, listVolume)

listVolumes' :: (MonadReader ScalewayEnv m, MonadIO m)
             => Page -> PerPage -> m (Response ByteString)
listVolumes' pageNumber nPerPage = listResource' pageNumber nPerPage listVolume

listVolumes :: (MonadReader ScalewayEnv m, MonadIO m)
            => Page -> PerPage -> m (Either String [Volume])
listVolumes pageNumber nPerPage = listResource pageNumber nPerPage listVolume

retrieveVolume' :: (MonadReader ScalewayEnv m, MonadIO m)
                => GetVolume -> m (Response ByteString)
retrieveVolume' = retrieveResource'

retrieveVolume :: (MonadReader ScalewayEnv m, MonadIO m)
               => GetVolume -> m (Either String Volume)
retrieveVolume = retrieveResource
