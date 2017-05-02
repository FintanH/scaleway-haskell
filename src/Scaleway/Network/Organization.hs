
module Scaleway.Network.Organization
    ( listOrganizations'
    , listOrganizations
    ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.ByteString.Lazy          (ByteString)
import           Network.Wreq                  (Response)
import           Scaleway.Internal.Request     (HeaderToken, Page, PerPage,
                                                listResource, listResource',
                                                retrieveResource,
                                                retrieveResource')
import           Scaleway.Internal.Types   (Organization, ScalewayRequest, listOrganization)

listOrganizations' :: (MonadReader ScalewayRequest m, MonadIO m)
                   => m (Response ByteString)
listOrganizations' = listResource' listOrganization

listOrganizations :: (MonadReader ScalewayRequest m, MonadIO m)
                  => m (Either String [Organization])
listOrganizations = listResource listOrganization
