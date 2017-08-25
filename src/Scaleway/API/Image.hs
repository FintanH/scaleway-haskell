{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Image
    ( ImageAPI
    , getImagesM
    , getImageM
    , putImageM
    , postImageM
    , deleteImageM
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
                                    Image, ImageCreate, ImageResult, Images)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureImageId = Capture "imageId" Text

type ImageAPI = "images" :> (
       ScalewayAuthToken
    :> QueryParam "per_page" PerPage
    :> QueryParam "page" Page
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
getImage_ :: Maybe XAuthToken -> Text -> ClientM Image
postImage_ :: Maybe XAuthToken -> ImageCreate -> ClientM ImageResult
putImage_ :: Maybe XAuthToken -> Text -> Image -> ClientM ImageResult
deleteImage_ :: Maybe XAuthToken -> Text -> ClientM ()
getImages_
  :<|> getImage_
  :<|> postImage_
  :<|> putImage_
  :<|> deleteImage_ = client imageAPI

getImagesM :: Maybe PerPage -> Maybe Page -> ScalewayClient Images
getImagesM = scalewayGetListRequest getImages_

getImageM :: Text -> ScalewayClient Image
getImageM = scalewayGetSingleRequest getImage_

postImageM :: ImageCreate -> ScalewayClient ImageResult
postImageM = scalewayPostRequest postImage_

putImageM :: Text -> Image -> ScalewayClient ImageResult
putImageM = scalewayPutRequest putImage_

deleteImageM :: Text -> ScalewayClient ()
deleteImageM = scalewayDeleteRequest deleteImage_
