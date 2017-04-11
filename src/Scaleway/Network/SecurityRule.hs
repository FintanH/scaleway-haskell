{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.SecurityRule
    ( listSecurityRules'
    , listSecurityRules
    , retrieveSecurityRule'
    , retrieveSecurityRule
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
import           Scaleway.Types.Resource   (GetSecurityRule, listSecurityRule)

listSecurityRules' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listSecurityRules' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listSecurityRule

listSecurityRules :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.SecurityRule])
listSecurityRules headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listSecurityRule

retrieveSecurityRule' :: HeaderToken -> Region -> GetSecurityRule -> IO (Response ByteString)
retrieveSecurityRule' headerToken region securityRule = retrieveResource' headerToken region securityRule

retrieveSecurityRule :: HeaderToken -> Region -> GetSecurityRule -> IO (Either String Get.SecurityRule)
retrieveSecurityRule headerToken region securityRule = retrieveResource headerToken region securityRule
