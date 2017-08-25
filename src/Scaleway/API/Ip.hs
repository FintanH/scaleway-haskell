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
                                    ScalewayClient, XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions, Ip,
                                    IpCreate, IpResult, Ips)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureIpId = Capture "ipId" Text

type IpAPI =
  "ips" :> (
       ScalewayAuthToken
    :> QueryParam "per_page" PerPage
    :> QueryParam "page" Page
    :> Get '[JSON] Ips

  :<|> ScalewayAuthToken
    :> CaptureIpId
    :> Get '[JSON] Ip

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] IpCreate
    :> Post '[JSON] IpResult

  :<|> ScalewayAuthToken
    :> CaptureIpId
    :> Put '[JSON] Ip

  :<|> ScalewayAuthToken
    :> CaptureIpId
    :> Delete '[JSON] ()
  )

ipAPI :: Proxy IpAPI
ipAPI = Proxy

getIps_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Ips
getIp_ :: Maybe XAuthToken -> Text -> ClientM Ip
postIp_ :: Maybe XAuthToken -> IpCreate -> ClientM IpResult
putIp_ :: Maybe XAuthToken -> Text -> ClientM Ip
deleteIp_ :: Maybe XAuthToken -> Text -> ClientM ()
getIps_
  :<|> getIp_
  :<|> postIp_
  :<|> putIp_
  :<|> deleteIp_ = client ipAPI

getIpsM :: Maybe PerPage -> Maybe Page -> ScalewayClient Ips
getIpsM = scalewayGetListRequest getIps_

getIpM :: Text -> ScalewayClient Ip
getIpM = scalewayGetSingleRequest getIp_

postIpM :: IpCreate -> ScalewayClient IpResult
postIpM = scalewayPostRequest postIp_

putIpM :: Text -> ScalewayClient Ip
putIpM = scalewayPutRequest putIp_

deleteIpM :: Text -> ScalewayClient ()
deleteIpM = scalewayDeleteRequest deleteIp_
