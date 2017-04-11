{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Token
    ( listTokens'
    , listTokens
    , retrieveToken'
    , retrieveToken
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
import           Scaleway.Types.Resource   (GetToken, listToken)

listTokens' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listTokens' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listToken

listTokens :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Token])
listTokens headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listToken

retrieveToken' :: HeaderToken -> Region -> GetToken -> IO (Response ByteString)
retrieveToken' headerToken region token = retrieveResource' headerToken region token

retrieveToken :: HeaderToken -> Region -> GetToken -> IO (Either String Get.Token)
retrieveToken headerToken region token = retrieveResource headerToken region token
