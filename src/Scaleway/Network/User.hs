
module Scaleway.Network.User
    ( listUsers'
    , listUsers
    , retrieveUser'
    , retrieveUser
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
import           Scaleway.Types.Resource       (GetUser, listUser)

listUsers' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listUsers' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listUser

listUsers :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.User])
listUsers headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listUser

retrieveUser' :: (MonadReader ScalewayEnv m, MonadIO m)
              => GetUser -> m (Response ByteString)
retrieveUser' = retrieveResource'

retrieveUser :: (MonadReader ScalewayEnv m, MonadIO m)
            => GetUser -> m (Either String Get.User)
retrieveUser = retrieveResource
