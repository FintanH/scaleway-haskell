{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Volume
    ( VolumeAPI
    , getVolumesM
    , getVolumeM
    , putVolumeM
    , postVolumeM
    , deleteVolumeM
    ) where

import           Data.Proxy        (Proxy (..))
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAuthToken,
                                    ScalewayComputeClient (..), XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (Volume, VolumeCreate, VolumeId,
                                    VolumeResult, Volumes)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureVolumeId = Capture "volumeId" VolumeId

type VolumeAPI =
  "volumes" :> (
       ScalewayAuthToken
    :> ParamPerPage
    :> ParamPage
    :> Get '[JSON] Volumes

  :<|> ScalewayAuthToken
    :> CaptureVolumeId
    :> Get '[JSON] Volume

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] VolumeCreate
    :> Post '[JSON] VolumeResult

  :<|> ScalewayAuthToken
    :> CaptureVolumeId
    :> ReqBody '[JSON] Volume
    :> Put '[JSON] Volume

  :<|> ScalewayAuthToken
    :> CaptureVolumeId
    :> Delete '[JSON] ()
  )

volumeAPI :: Proxy VolumeAPI
volumeAPI = Proxy

getVolumes_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Volumes
getVolume_ :: Maybe XAuthToken -> VolumeId -> ClientM Volume
postVolume_ :: Maybe XAuthToken -> VolumeCreate -> ClientM VolumeResult
putVolume_ :: Maybe XAuthToken -> VolumeId -> Volume -> ClientM Volume
deleteVolume_ :: Maybe XAuthToken -> VolumeId -> ClientM ()
getVolumes_
  :<|> getVolume_
  :<|> postVolume_
  :<|> putVolume_
  :<|> deleteVolume_ = client volumeAPI

getVolumesM :: Maybe PerPage -> Maybe Page -> ScalewayComputeClient Volumes
getVolumesM perPage = ScalewayCompute . scalewayGetListRequest getVolumes_ perPage

getVolumeM :: VolumeId -> ScalewayComputeClient Volume
getVolumeM = ScalewayCompute . scalewayGetSingleRequest getVolume_

postVolumeM :: VolumeCreate -> ScalewayComputeClient VolumeResult
postVolumeM = ScalewayCompute . scalewayPostRequest postVolume_

putVolumeM :: VolumeId -> Volume -> ScalewayComputeClient Volume
putVolumeM i = ScalewayCompute . scalewayPutRequest putVolume_ i

deleteVolumeM :: VolumeId -> ScalewayComputeClient ()
deleteVolumeM = ScalewayCompute . scalewayDeleteRequest deleteVolume_
