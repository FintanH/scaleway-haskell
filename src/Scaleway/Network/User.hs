
module Scaleway.Network.User
    ( listUsers'
    , listUsers
    , retrieveUser'
    , retrieveUser
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetUser, listUser)

listUsers' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listUsers' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listUser

listUsers :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.User])
listUsers headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listUser

retrieveUser' :: HeaderToken -> Region -> GetUser -> IO (Response ByteString)
retrieveUser' headerToken region user = retrieveResource' headerToken region user

retrieveUser :: HeaderToken -> Region -> GetUser -> IO (Either String Get.User)
retrieveUser headerToken region user = retrieveResource headerToken region user
