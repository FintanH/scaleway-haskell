
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

listSecurityRules' :: (MonadReader ScalewayEnv m, MonadIO m)
                   => Page -> PerPage -> m (Response ByteString)
listSecurityRules' pageNumber nPerPage = listResource' pageNumber nPerPage listSecurityRule

listSecurityRules :: (MonadReader ScalewayEnv m, MonadIO m)
                  => Page -> PerPage -> m (Either String [Get.SecurityRule])
listSecurityRules pageNumber nPerPage = listResource pageNumber nPerPage listSecurityRule

retrieveSecurityRule' :: (MonadReader ScalewayEnv m, MonadIO m)
                      => GetSecurityRule -> m (Response ByteString)
retrieveSecurityRule' = retrieveResource'

retrieveSecurityRule :: (MonadReader ScalewayEnv m, MonadIO m)
                    => GetSecurityRule -> m (Either String Get.SecurityRule)
retrieveSecurityRule = retrieveResource
