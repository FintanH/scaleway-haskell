
module Scaleway.Network.SecurityGroup
    ( listSecurityGroups'
    , listSecurityGroups
    , retrieveSecurityGroup'
    , retrieveSecurityGroup
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetSecurityGroup, listSecurityGroup)

listSecurityGroups' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listSecurityGroups' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listSecurityGroup

listSecurityGroups :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.SecurityGroup])
listSecurityGroups headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listSecurityGroup

retrieveSecurityGroup' :: HeaderToken -> Region -> GetSecurityGroup -> IO (Response ByteString)
retrieveSecurityGroup' headerToken region securityGroup = retrieveResource' headerToken region securityGroup

retrieveSecurityGroup :: HeaderToken -> Region -> GetSecurityGroup -> IO (Either String Get.SecurityGroup)
retrieveSecurityGroup headerToken region securityGroup = retrieveResource headerToken region securityGroup
