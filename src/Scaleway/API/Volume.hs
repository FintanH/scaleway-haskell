{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
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
import           Data.Text         (Text)
import           Scaleway.API.Core (Page, PerPage, ScalewayAuthToken,
                                    ScalewayClient, XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions,
                                    Volume, VolumeCreate, VolumeResult, Volumes)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureVolumeId = Capture "volumeId" Text

type VolumeAPI =
  "volumes" :> (
       ScalewayAuthToken
    :> QueryParam "per_page" PerPage
    :> QueryParam "page" Page
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
getVolume_ :: Maybe XAuthToken -> Text -> ClientM Volume
postVolume_ :: Maybe XAuthToken -> VolumeCreate -> ClientM VolumeResult
putVolume_ :: Maybe XAuthToken -> Text -> Volume -> ClientM Volume
deleteVolume_ :: Maybe XAuthToken -> Text -> ClientM ()
getVolumes_
  :<|> getVolume_
  :<|> postVolume_
  :<|> putVolume_
  :<|> deleteVolume_ = client volumeAPI

getVolumesM :: Maybe PerPage -> Maybe Page -> ScalewayClient Volumes
getVolumesM = scalewayGetListRequest getVolumes_

getVolumeM :: Text -> ScalewayClient Volume
getVolumeM = scalewayGetSingleRequest getVolume_

postVolumeM :: VolumeCreate -> ScalewayClient VolumeResult
postVolumeM = scalewayPostRequest postVolume_

putVolumeM :: Text -> Volume -> ScalewayClient Volume
putVolumeM = scalewayPutRequest putVolume_

deleteVolumeM :: Text -> ScalewayClient ()
deleteVolumeM = scalewayDeleteRequest deleteVolume_
