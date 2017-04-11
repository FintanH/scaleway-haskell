{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Snapshot
    ( listSnapshots'
    , listSnapshots
    , retrieveSnapshot'
    , retrieveSnapshot
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
import           Scaleway.Types.Resource   (GetSnapshot, listSnapshot)

listSnapshots' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listSnapshots' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listSnapshot

listSnapshots :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Snapshot])
listSnapshots headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listSnapshot

retrieveSnapshot' :: HeaderToken -> Region -> GetSnapshot -> IO (Response ByteString)
retrieveSnapshot' headerToken region snapshot = retrieveResource' headerToken region snapshot

retrieveSnapshot :: HeaderToken -> Region -> GetSnapshot -> IO (Either String Get.Snapshot)
retrieveSnapshot headerToken region snapshot = retrieveResource headerToken region snapshot
