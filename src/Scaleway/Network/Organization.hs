{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Organization
    ( listOrganizations'
    , listOrganizations
    , retrieveOrganization'
    , retrieveOrganization
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
import           Scaleway.Types.Resource   (GetOrganization, listOrganization)

listOrganizations' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listOrganizations' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listOrganization

listOrganizations :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Organization])
listOrganizations headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listOrganization

retrieveOrganization' :: HeaderToken -> Region -> GetOrganization -> IO (Response ByteString)
retrieveOrganization' headerToken region organization = retrieveResource' headerToken region organization

retrieveOrganization :: HeaderToken -> Region -> GetOrganization -> IO (Either String Get.Organization)
retrieveOrganization headerToken region organization = retrieveResource headerToken region organization
