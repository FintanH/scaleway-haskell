
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
import           Scaleway.Internal.Types   (GetUser, User, ScalewayRequest, listUser)

listUsers' :: (MonadReader ScalewayRequest m, MonadIO m)
           => m (Response ByteString)
listUsers' = listResource' listUser

listUsers :: (MonadReader ScalewayRequest m, MonadIO m)
          => m (Either String [User])
listUsers = listResource listUser

retrieveUser' :: (MonadReader ScalewayRequest m, MonadIO m)
              => GetUser -> m (Response ByteString)
retrieveUser' = retrieveResource'

retrieveUser :: (MonadReader ScalewayRequest m, MonadIO m)
            => GetUser -> m (Either String User)
retrieveUser = retrieveResource
