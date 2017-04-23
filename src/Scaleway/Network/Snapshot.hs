
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
import           Scaleway.Internal.ScalewayEnv (ScalewayEnv)
import qualified Scaleway.Types.Get            as Get
import qualified Scaleway.Types.Get            as Get
import           Scaleway.Types.Internal       (Region)
import           Scaleway.Types.Resource       (GetSnapshot, listSnapshot)

listSnapshots' :: (MonadReader ScalewayEnv m, MonadIO m)
               => Page -> PerPage -> m (Response ByteString)
listSnapshots' pageNumber nPerPage = listResource' pageNumber nPerPage listSnapshot

listSnapshots :: (MonadReader ScalewayEnv m, MonadIO m)
              => Page -> PerPage -> m (Either String [Get.Snapshot])
listSnapshots pageNumber nPerPage = listResource pageNumber nPerPage listSnapshot

retrieveSnapshot' :: (MonadReader ScalewayEnv m, MonadIO m)
                  => GetSnapshot -> m (Response ByteString)
retrieveSnapshot' = retrieveResource'

retrieveSnapshot :: (MonadReader ScalewayEnv m, MonadIO m)
                 => GetSnapshot -> m (Either String Get.Snapshot)
retrieveSnapshot = retrieveResource
