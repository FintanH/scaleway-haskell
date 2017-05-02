
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
import           Scaleway.Internal.Types   (Organization, ScalewayEnv, listOrganization)

listOrganizations' :: (MonadReader ScalewayEnv m, MonadIO m)
                   => Page -> PerPage -> m (Response ByteString)
listOrganizations' pageNumber nPerPage = listResource' pageNumber nPerPage listOrganization

listOrganizations :: (MonadReader ScalewayEnv m, MonadIO m)
                  => Page -> PerPage -> m (Either String [Organization])
listOrganizations pageNumber nPerPage = listResource pageNumber nPerPage listOrganization
