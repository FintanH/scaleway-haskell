{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Ip
    ( IpAPI
    , getIpsM
    , getIpM
    , putIpM
    , postIpM
    , deleteIpM
    ) where

import           Data.Proxy        (Proxy (..))
import           Data.Text         (Text)
import           Scaleway.API.Core (Page, PerPage, ScalewayAuthToken,
                                    ScalewayClient, XAuthToken, ParamPerPage, ParamPage,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions, Ip,
                                    IpCreate, IpResult, Ips, IpId)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureIpId = Capture "ipId" IpId

type IpAPI =
  "ips" :> (
       ScalewayAuthToken
    :> ParamPerPage
    :> ParamPage
    :> Get '[JSON] Ips

  :<|> ScalewayAuthToken
    :> CaptureIpId
    :> Get '[JSON] Ip

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] IpCreate
    :> Post '[JSON] IpResult

  :<|> ScalewayAuthToken
    :> CaptureIpId
    :> ReqBody '[JSON] Ip
    :> Put '[JSON] IpResult

  :<|> ScalewayAuthToken
    :> CaptureIpId
    :> Delete '[JSON] ()
  )

ipAPI :: Proxy IpAPI
ipAPI = Proxy

getIps_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Ips
getIp_ :: Maybe XAuthToken -> IpId -> ClientM Ip
postIp_ :: Maybe XAuthToken -> IpCreate -> ClientM IpResult
putIp_ :: Maybe XAuthToken -> IpId -> Ip -> ClientM IpResult
deleteIp_ :: Maybe XAuthToken -> IpId -> ClientM ()
getIps_
  :<|> getIp_
  :<|> postIp_
  :<|> putIp_
  :<|> deleteIp_ = client ipAPI

getIpsM :: Maybe PerPage -> Maybe Page -> ScalewayClient Ips
getIpsM = scalewayGetListRequest getIps_

getIpM :: IpId -> ScalewayClient Ip
getIpM = scalewayGetSingleRequest getIp_

postIpM :: IpCreate -> ScalewayClient IpResult
postIpM = scalewayPostRequest postIp_

putIpM :: IpId -> Ip -> ScalewayClient IpResult
putIpM = scalewayPutRequest putIp_

deleteIpM :: IpId -> ScalewayClient ()
deleteIpM = scalewayDeleteRequest deleteIp_
