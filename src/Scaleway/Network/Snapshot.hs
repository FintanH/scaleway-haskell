
module Scaleway.Network.Snapshot
    ( listSnapshots'
    , listSnapshots
    , retrieveSnapshot'
    , retrieveSnapshot
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetSnapshot, listSnapshot)

listSnapshots' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listSnapshots' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listSnapshot

listSnapshots :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Snapshot])
listSnapshots headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listSnapshot

retrieveSnapshot' :: HeaderToken -> Region -> GetSnapshot -> IO (Response ByteString)
retrieveSnapshot' headerToken region snapshot = retrieveResource' headerToken region snapshot

retrieveSnapshot :: HeaderToken -> Region -> GetSnapshot -> IO (Either String Get.Snapshot)
retrieveSnapshot headerToken region snapshot = retrieveResource headerToken region snapshot
