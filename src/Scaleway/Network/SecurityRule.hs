
module Scaleway.Network.SecurityRule
    ( listSecurityRules'
    , listSecurityRules
    , retrieveSecurityRule'
    , retrieveSecurityRule
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
import           Scaleway.Types.Resource       (GetSecurityRule,
                                                listSecurityRule)

listSecurityRules' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listSecurityRules' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listSecurityRule

listSecurityRules :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.SecurityRule])
listSecurityRules headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listSecurityRule

retrieveSecurityRule' :: (MonadReader ScalewayEnv m, MonadIO m)
                      => GetSecurityRule -> m (Response ByteString)
retrieveSecurityRule' = retrieveResource'

retrieveSecurityRule :: (MonadReader ScalewayEnv m, MonadIO m)
                    => GetSecurityRule -> m (Either String Get.SecurityRule)
retrieveSecurityRule = retrieveResource
