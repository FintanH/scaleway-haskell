
module Scaleway.Network.Token
    ( listTokens'
    , listTokens
    , retrieveToken'
    , retrieveToken
    ) where

import           Data.ByteString.Lazy      (ByteString)
import           Network.Wreq              (Response)
import           Scaleway.Internal.Request (HeaderToken, Page, PerPage,
                                            listResource, listResource',
                                            retrieveResource, retrieveResource')
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal   (Region)
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Resource   (GetToken, listToken)

listTokens' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listTokens' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listToken

listTokens :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Token])
listTokens headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listToken

retrieveToken' :: HeaderToken -> Region -> GetToken -> IO (Response ByteString)
retrieveToken' headerToken region token = retrieveResource' headerToken region token

retrieveToken :: HeaderToken -> Region -> GetToken -> IO (Either String Get.Token)
retrieveToken headerToken region token = retrieveResource headerToken region token
