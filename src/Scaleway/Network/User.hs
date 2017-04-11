{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.User
    ( listUsers'
    , listUsers
    , retrieveUser'
    , retrieveUser
    ) where

import           Control.Lens
import           Data.Aeson                (Value, eitherDecode, withObject,
                                            (.:))
import           Data.Aeson.Types          (parseEither, parseJSON, toJSON)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, unpack)
import           Network.Wreq              (Response, defaults, deleteWith,
                                            getWith, postWith, responseBody)
import           Scaleway.Internal.Request
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal
import qualified Scaleway.Types.Post       as Post
import           Scaleway.Types.Resource   (GetUser, listUser)

listUsers' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listUsers' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listUser

listUsers :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.User])
listUsers headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listUser

retrieveUser' :: HeaderToken -> Region -> GetUser -> IO (Response ByteString)
retrieveUser' headerToken region user = retrieveResource' headerToken region user

retrieveUser :: HeaderToken -> Region -> GetUser -> IO (Either String Get.User)
retrieveUser headerToken region user = retrieveResource headerToken region user
