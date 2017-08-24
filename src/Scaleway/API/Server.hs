{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Server
    ( ServerAPI
    , getServersM
    , getServerM
    , putServerM
    , postServerM
    , deleteServerM
    ) where

import           Data.Proxy        (Proxy (..))
import           Data.Text         (Text)
import           Scaleway.API.Core (Page, PerPage, ScalewayAuthToken,
                                    ScalewayClient, XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (Server, ServerCreate, ServerResult, Servers)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody,
                                    ToHttpApiData (toUrlPiece))
import           Servant.Client    (ClientM, client)

type ServerAPI =
       "servers" :> ScalewayAuthToken
                 :> QueryParam "per_page" PerPage
                 :> QueryParam "page" Page
                 :> Get '[JSON] Servers
  :<|> "servers" :> ScalewayAuthToken
                 :> Capture "serverId" Text
                 :> QueryParam "per_page" PerPage
                 :> QueryParam "page" Page
                 :> Get '[JSON] Server
  :<|> "servers" :> ScalewayAuthToken
                 :> ReqBody '[JSON] ServerCreate
                 :> Post '[JSON] ServerResult
  :<|> "servers" :> ScalewayAuthToken
                 :> Capture "serverId" Text
                 :> Put '[JSON] Server
  :<|> "servers" :> ScalewayAuthToken
                 :> Capture "serverId" Text
                 :> Delete '[JSON] ()

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

getServers_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Servers
getServer_ :: Maybe XAuthToken -> Text -> Maybe PerPage -> Maybe Page -> ClientM Server
postServer_ :: Maybe XAuthToken -> ServerCreate -> ClientM ServerResult
putServer_ :: Maybe XAuthToken -> Text -> ClientM Server
deleteServer_ :: Maybe XAuthToken -> Text -> ClientM ()
getServers_
  :<|> getServer_
  :<|> postServer_
  :<|> putServer_
  :<|> deleteServer_ = client serverAPI

getServersM :: Maybe PerPage -> Maybe Page -> ScalewayClient Servers
getServersM = scalewayGetListRequest getServers_

getServerM :: Text -> Maybe PerPage -> Maybe Page -> ScalewayClient Server
getServerM = scalewayGetSingleRequest getServer_

postServerM :: ServerCreate -> ScalewayClient ServerResult
postServerM = scalewayPostRequest postServer_

putServerM :: Text -> ScalewayClient Server
putServerM = scalewayPutRequest putServer_

deleteServerM :: Text -> ScalewayClient ()
deleteServerM = scalewayDeleteRequest deleteServer_
