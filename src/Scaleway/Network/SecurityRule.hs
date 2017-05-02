
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
import           Scaleway.Internal.Types   (GetSecurityRule, SecurityRule, ScalewayRequest, listSecurityRule)

listSecurityRules' :: (MonadReader ScalewayRequest m, MonadIO m)
                   => m (Response ByteString)
listSecurityRules' = listResource' listSecurityRule

listSecurityRules :: (MonadReader ScalewayRequest m, MonadIO m)
                  => m (Either String [SecurityRule])
listSecurityRules = listResource listSecurityRule

retrieveSecurityRule' :: (MonadReader ScalewayRequest m, MonadIO m)
                      => GetSecurityRule -> m (Response ByteString)
retrieveSecurityRule' = retrieveResource'

retrieveSecurityRule :: (MonadReader ScalewayRequest m, MonadIO m)
                    => GetSecurityRule -> m (Either String SecurityRule)
retrieveSecurityRule = retrieveResource
