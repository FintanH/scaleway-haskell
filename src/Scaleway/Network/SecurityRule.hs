
module Scaleway.Network.SecurityRule
    ( listSecurityRules'
    , listSecurityRules
    , retrieveSecurityRule'
    , retrieveSecurityRule
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetSecurityRule, listSecurityRule)

listSecurityRules' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listSecurityRules' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listSecurityRule

listSecurityRules :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.SecurityRule])
listSecurityRules headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listSecurityRule

retrieveSecurityRule' :: HeaderToken -> Region -> GetSecurityRule -> IO (Response ByteString)
retrieveSecurityRule' headerToken region securityRule = retrieveResource' headerToken region securityRule

retrieveSecurityRule :: HeaderToken -> Region -> GetSecurityRule -> IO (Either String Get.SecurityRule)
retrieveSecurityRule headerToken region securityRule = retrieveResource headerToken region securityRule
