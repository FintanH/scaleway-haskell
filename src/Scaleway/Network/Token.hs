
module Scaleway.Network.Token
    ( listTokens'
    , listTokens
    , retrieveToken'
    , retrieveToken
    ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.ByteString.Lazy          (ByteString)
import           Network.Wreq                  (Response)
import           Scaleway.Internal.Request     (HeaderToken, Page, PerPage,
                                                listResource, listResource',
                                                retrieveResource,
                                                retrieveResource')
import           Scaleway.Internal.Types   (GetToken, Token, ScalewayRequest, listToken)

listTokens' :: (MonadReader ScalewayRequest m, MonadIO m)
            => m (Response ByteString)
listTokens' = listResource' listToken

listTokens :: (MonadReader ScalewayRequest m, MonadIO m)
           => m (Either String [Token])
listTokens = listResource listToken

retrieveToken' :: (MonadReader ScalewayRequest m, MonadIO m)
               => GetToken -> m (Response ByteString)
retrieveToken' = retrieveResource'

retrieveToken :: (MonadReader ScalewayRequest m, MonadIO m)
              => GetToken -> m (Either String Token)
retrieveToken = retrieveResource
