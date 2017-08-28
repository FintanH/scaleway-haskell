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
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAuthToken,
                                    ScalewayComputeClient (..), XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions,
                                    Server, ServerCreate, ServerId,
                                    ServerResult, Servers)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureServerId = Capture "serverId" ServerId

type ServerAPI =
  "servers" :> (
       ScalewayAuthToken
    :> ParamPerPage
    :> ParamPage
    :> Get '[JSON] Servers

  :<|> ScalewayAuthToken
    :> CaptureServerId
    :> Get '[JSON] ServerResult

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] ServerCreate
    :> Post '[JSON] ServerResult

  :<|> ScalewayAuthToken
    :> CaptureServerId
    :> ReqBody '[JSON] Server
    :> Put '[JSON] ServerResult

  :<|> ScalewayAuthToken
    :> CaptureServerId
    :> Delete '[JSON] ()

  :<|> ScalewayAuthToken
    :> CaptureServerId
    :> "action"
    :> Get '[JSON] Actions

  :<|> ScalewayAuthToken
    :> CaptureServerId
    :> "action"
    :> ReqBody '[JSON] ActionRequest
    :> Post '[JSON] ActionResponse
  )

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

getServers_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Servers
getServer_ :: Maybe XAuthToken -> ServerId -> ClientM ServerResult
postServer_ :: Maybe XAuthToken -> ServerCreate -> ClientM ServerResult
putServer_ :: Maybe XAuthToken -> ServerId -> Server -> ClientM ServerResult
deleteServer_ :: Maybe XAuthToken -> ServerId -> ClientM ()
getActions_ :: Maybe XAuthToken -> ServerId -> ClientM Actions
postAction_ :: Maybe XAuthToken -> ServerId -> ActionRequest -> ClientM ActionResponse
getServers_
  :<|> getServer_
  :<|> postServer_
  :<|> putServer_
  :<|> deleteServer_
  :<|> getActions_
  :<|> postAction_ = client serverAPI

getServersM :: Maybe PerPage -> Maybe Page -> ScalewayComputeClient Servers
getServersM perPage = ScalewayCompute . scalewayGetListRequest getServers_ perPage

getServerM :: ServerId -> ScalewayComputeClient ServerResult
getServerM = ScalewayCompute . scalewayGetSingleRequest getServer_

postServerM :: ServerCreate -> ScalewayComputeClient ServerResult
postServerM = ScalewayCompute . scalewayPostRequest postServer_

putServerM :: ServerId -> Server -> ScalewayComputeClient ServerResult
putServerM i = ScalewayCompute . scalewayPutRequest putServer_ i

deleteServerM :: ServerId -> ScalewayComputeClient ()
deleteServerM = ScalewayCompute . scalewayDeleteRequest deleteServer_

getActionsM :: ServerId -> ScalewayComputeClient Actions
getActionsM = ScalewayCompute . scalewayGetSingleRequest getActions_

postActionM :: ServerId -> ActionRequest -> ScalewayComputeClient ActionResponse
postActionM serverId = ScalewayCompute . scalewayPostRequest (\auth -> postAction_ auth serverId)
