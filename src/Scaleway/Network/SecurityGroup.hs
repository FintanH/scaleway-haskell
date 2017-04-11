{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.SecurityGroup
    ( listSecurityGroups'
    , listSecurityGroups
    , retrieveSecurityGroup'
    , retrieveSecurityGroup
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
import           Scaleway.Types.Resource   (GetSecurityGroup, listSecurityGroup)

listSecurityGroups' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listSecurityGroups' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listSecurityGroup

listSecurityGroups :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.SecurityGroup])
listSecurityGroups headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listSecurityGroup

retrieveSecurityGroup' :: HeaderToken -> Region -> GetSecurityGroup -> IO (Response ByteString)
retrieveSecurityGroup' headerToken region securityGroup = retrieveResource' headerToken region securityGroup

retrieveSecurityGroup :: HeaderToken -> Region -> GetSecurityGroup -> IO (Either String Get.SecurityGroup)
retrieveSecurityGroup headerToken region securityGroup = retrieveResource headerToken region securityGroup
