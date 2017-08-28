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
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAuthToken,
                                    ScalewayComputeClient (..), XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (Ip, IpCreate, IpId, IpResult, Ips)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, ReqBody)
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

getIpsM :: Maybe PerPage -> Maybe Page -> ScalewayComputeClient Ips
getIpsM perPage = ScalewayCompute . scalewayGetListRequest getIps_ perPage

getIpM :: IpId -> ScalewayComputeClient Ip
getIpM = ScalewayCompute . scalewayGetSingleRequest getIp_

postIpM :: IpCreate -> ScalewayComputeClient IpResult
postIpM = ScalewayCompute . scalewayPostRequest postIp_

putIpM :: IpId -> Ip -> ScalewayComputeClient IpResult
putIpM i = ScalewayCompute . scalewayPutRequest putIp_ i

deleteIpM :: IpId -> ScalewayComputeClient ()
deleteIpM = ScalewayCompute . scalewayDeleteRequest deleteIp_
