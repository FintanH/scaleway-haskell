
module Scaleway.Network.Organization
    ( listOrganizations'
    , listOrganizations
    , retrieveOrganization'
    , retrieveOrganization
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
import           Scaleway.Types.Resource       (GetOrganization,
                                                listOrganization)

listOrganizations' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listOrganizations' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listOrganization

listOrganizations :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Organization])
listOrganizations headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listOrganization

retrieveOrganization' :: (MonadReader ScalewayEnv m, MonadIO m)
                      => GetOrganization -> m (Response ByteString)
retrieveOrganization' = retrieveResource'

retrieveOrganization :: (MonadReader ScalewayEnv m, MonadIO m)
                     => GetOrganization -> m (Either String Get.Organization)
retrieveOrganization = retrieveResource
