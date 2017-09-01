{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Scaleway.API.Image
    ( ImageAPI
    , getImagesM
    , getImageM
    , putImageM
    , postImageM
    , deleteImageM
    ) where

import           Data.Proxy        (Proxy (..))
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAuthToken,
                                    ScalewayComputeClient (..), XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (Image, ImageCreate, ImageId, ImageResult,
                                    Images)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureImageId = Capture "imageId" ImageId

type ImageAPI = "images" :> (
       ScalewayAuthToken
    :> ParamPerPage
    :> ParamPage
    :> Get '[JSON] Images

  :<|> ScalewayAuthToken
    :> CaptureImageId
    :> Get '[JSON] Image

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] ImageCreate
    :> Post '[JSON] ImageResult

  :<|> ScalewayAuthToken
    :> CaptureImageId
    :> ReqBody '[JSON] Image
    :> Put '[JSON] ImageResult

  :<|> ScalewayAuthToken
    :> CaptureImageId
    :> Delete '[JSON] ()
  )

imageAPI :: Proxy ImageAPI
imageAPI = Proxy

getImages_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Images
getImage_ :: Maybe XAuthToken -> ImageId -> ClientM Image
postImage_ :: Maybe XAuthToken -> ImageCreate -> ClientM ImageResult
putImage_ :: Maybe XAuthToken -> ImageId -> Image -> ClientM ImageResult
deleteImage_ :: Maybe XAuthToken -> ImageId -> ClientM ()
getImages_
  :<|> getImage_
  :<|> postImage_
  :<|> putImage_
  :<|> deleteImage_ = client imageAPI

getImagesM :: Maybe PerPage -> Maybe Page -> ScalewayComputeClient Images
getImagesM perPage = ScalewayCompute . scalewayGetListRequest getImages_ perPage

getImageM :: ImageId -> ScalewayComputeClient Image
getImageM = ScalewayCompute . scalewayGetSingleRequest getImage_

postImageM :: ImageCreate -> ScalewayComputeClient ImageResult
postImageM = ScalewayCompute . scalewayPostRequest postImage_

putImageM :: ImageId -> Image -> ScalewayComputeClient ImageResult
putImageM i = ScalewayCompute . scalewayPutRequest putImage_ i

deleteImageM :: ImageId -> ScalewayComputeClient ()
deleteImageM = ScalewayCompute . scalewayDeleteRequest deleteImage_
