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
    , getActionsM
    , postActionM
    ) where

import           Data.Proxy        (Proxy (..))
import           Data.Text         (Text)
import           Scaleway.API.Core (Page, PerPage, ScalewayAuthToken,
                                    ScalewayClient, XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions,
                                    Server, ServerCreate, ServerResult, Servers)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody,
                                    ToHttpApiData (toUrlPiece))
import           Servant.Client    (ClientM, client)

type CaptureServerId = Capture "serverId" Text

type ServerAPI =
       "servers" :> ScalewayAuthToken
                 :> QueryParam "per_page" PerPage
                 :> QueryParam "page" Page
                 :> Get '[JSON] Servers
  :<|> "servers" :> ScalewayAuthToken
                 :> CaptureServerId
                 :> Get '[JSON] Server
  :<|> "servers" :> ScalewayAuthToken
                 :> ReqBody '[JSON] ServerCreate
                 :> Post '[JSON] ServerResult
  :<|> "servers" :> ScalewayAuthToken
                 :> CaptureServerId
                 :> Put '[JSON] Server
  :<|> "servers" :> ScalewayAuthToken
                 :> CaptureServerId
                 :> Delete '[JSON] ()
  :<|> "servers" :> ScalewayAuthToken
                 :> CaptureServerId
                 :> "action"
                 :> Get '[JSON] Actions
  :<|> "servers" :> ScalewayAuthToken
                 :> CaptureServerId
                 :> ReqBody '[JSON] ActionRequest
                 :> Post '[JSON] ActionResponse

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

getServers_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Servers
getServer_ :: Maybe XAuthToken -> Text -> ClientM Server
postServer_ :: Maybe XAuthToken -> ServerCreate -> ClientM ServerResult
putServer_ :: Maybe XAuthToken -> Text -> ClientM Server
deleteServer_ :: Maybe XAuthToken -> Text -> ClientM ()
getActions_ :: Maybe XAuthToken -> Text -> ClientM Actions
postAction_ :: Maybe XAuthToken -> Text -> ActionRequest -> ClientM ActionResponse
getServers_
  :<|> getServer_
  :<|> postServer_
  :<|> putServer_
  :<|> deleteServer_
  :<|> getActions_
  :<|> postAction_ = client serverAPI

getServersM :: Maybe PerPage -> Maybe Page -> ScalewayClient Servers
getServersM = scalewayGetListRequest getServers_

getServerM :: Text -> ScalewayClient Server
getServerM = scalewayGetSingleRequest getServer_

postServerM :: ServerCreate -> ScalewayClient ServerResult
postServerM = scalewayPostRequest postServer_

putServerM :: Text -> ScalewayClient Server
putServerM = scalewayPutRequest putServer_

deleteServerM :: Text -> ScalewayClient ()
deleteServerM = scalewayDeleteRequest deleteServer_

getActionsM :: Text -> ScalewayClient Actions
getActionsM = scalewayGetSingleRequest getActions_

postActionM :: Text -> ActionRequest -> ScalewayClient ActionResponse
postActionM serverId = scalewayPostRequest (\auth -> postAction_ auth serverId)
