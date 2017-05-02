
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
import           Scaleway.Internal.Types   (GetSecurityGroup, SecurityGroup, ScalewayRequest, listSecurityGroup)

listSecurityGroups' :: (MonadReader ScalewayRequest m, MonadIO m)
                    => m (Response ByteString)
listSecurityGroups' = listResource' listSecurityGroup

listSecurityGroups :: (MonadReader ScalewayRequest m, MonadIO m)
                   => m (Either String [SecurityGroup])
listSecurityGroups = listResource listSecurityGroup

retrieveSecurityGroup' :: (MonadReader ScalewayRequest m, MonadIO m)
                       => GetSecurityGroup -> m (Response ByteString)
retrieveSecurityGroup' = retrieveResource'

retrieveSecurityGroup :: (MonadReader ScalewayRequest m, MonadIO m)
                      => GetSecurityGroup -> m (Either String SecurityGroup)
retrieveSecurityGroup = retrieveResource
