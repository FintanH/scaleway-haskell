
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
import           Scaleway.Internal.Types   (GetUser, User, ScalewayEnv, listUser)

listUsers' :: (MonadReader ScalewayEnv m, MonadIO m)
           => Page -> PerPage -> m (Response ByteString)
listUsers' pageNumber nPerPage = listResource' pageNumber nPerPage listUser

listUsers :: (MonadReader ScalewayEnv m, MonadIO m)
          => Page -> PerPage -> m (Either String [User])
listUsers pageNumber nPerPage = listResource pageNumber nPerPage listUser

retrieveUser' :: (MonadReader ScalewayEnv m, MonadIO m)
              => GetUser -> m (Response ByteString)
retrieveUser' = retrieveResource'

retrieveUser :: (MonadReader ScalewayEnv m, MonadIO m)
            => GetUser -> m (Either String User)
retrieveUser = retrieveResource
