
module Scaleway.Network.Organization
    ( listOrganizations'
    , listOrganizations
    , retrieveOrganization'
    , retrieveOrganization
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetOrganization, listOrganization)

listOrganizations' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listOrganizations' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listOrganization

listOrganizations :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Organization])
listOrganizations headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listOrganization

retrieveOrganization' :: HeaderToken -> Region -> GetOrganization -> IO (Response ByteString)
retrieveOrganization' headerToken region organization = retrieveResource' headerToken region organization

retrieveOrganization :: HeaderToken -> Region -> GetOrganization -> IO (Either String Get.Organization)
retrieveOrganization headerToken region organization = retrieveResource headerToken region organization
