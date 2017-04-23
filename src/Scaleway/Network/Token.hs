
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
import           Scaleway.Internal.ScalewayEnv (ScalewayEnv)
import qualified Scaleway.Types.Get            as Get
import qualified Scaleway.Types.Get            as Get
import           Scaleway.Types.Internal       (Region)
import           Scaleway.Types.Resource       (GetToken, listToken)

listTokens' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listTokens' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listToken

listTokens :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Token])
listTokens headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listToken

retrieveToken' :: (MonadReader ScalewayEnv m, MonadIO m)
               => GetToken -> m (Response ByteString)
retrieveToken' = retrieveResource'

retrieveToken :: (MonadReader ScalewayEnv m, MonadIO m)
              => GetToken -> m (Either String Get.Token)
retrieveToken = retrieveResource
