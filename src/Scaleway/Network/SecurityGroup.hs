
module Scaleway.Network.SecurityGroup
    ( listSecurityGroups'
    , listSecurityGroups
    , retrieveSecurityGroup'
    , retrieveSecurityGroup
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
import           Scaleway.Types.Resource       (GetSecurityGroup,
                                                listSecurityGroup)

listSecurityGroups' :: (MonadReader ScalewayEnv m, MonadIO m)
                    => Page -> PerPage -> m (Response ByteString)
listSecurityGroups' pageNumber nPerPage = listResource' pageNumber nPerPage listSecurityGroup

listSecurityGroups :: (MonadReader ScalewayEnv m, MonadIO m)
                   => Page -> PerPage -> m (Either String [Get.SecurityGroup])
listSecurityGroups pageNumber nPerPage = listResource pageNumber nPerPage listSecurityGroup

retrieveSecurityGroup' :: (MonadReader ScalewayEnv m, MonadIO m)
                       => GetSecurityGroup -> m (Response ByteString)
retrieveSecurityGroup' = retrieveResource'

retrieveSecurityGroup :: (MonadReader ScalewayEnv m, MonadIO m)
                      => GetSecurityGroup -> m (Either String Get.SecurityGroup)
retrieveSecurityGroup = retrieveResource
