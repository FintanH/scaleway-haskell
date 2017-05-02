
module Scaleway.Network.Snapshot
    ( listSnapshots'
    , listSnapshots
    , retrieveSnapshot'
    , retrieveSnapshot
    ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.ByteString.Lazy          (ByteString)
import           Network.Wreq                  (Response)
import           Scaleway.Internal.Request     (HeaderToken, Page, PerPage,
                                                listResource, listResource',
                                                retrieveResource,
                                                retrieveResource')
import           Scaleway.Internal.Types   (GetSnapshot, Snapshot, ScalewayRequest, listSnapshot)

listSnapshots' :: (MonadReader ScalewayRequest m, MonadIO m)
               => m (Response ByteString)
listSnapshots' = listResource' listSnapshot

listSnapshots :: (MonadReader ScalewayRequest m, MonadIO m)
              => m (Either String [Snapshot])
listSnapshots = listResource listSnapshot

retrieveSnapshot' :: (MonadReader ScalewayRequest m, MonadIO m)
                  => GetSnapshot -> m (Response ByteString)
retrieveSnapshot' = retrieveResource'

retrieveSnapshot :: (MonadReader ScalewayRequest m, MonadIO m)
                 => GetSnapshot -> m (Either String Snapshot)
retrieveSnapshot = retrieveResource
