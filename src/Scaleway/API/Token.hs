{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Token
    ( TokenAPI
    , getTokensM
    , getTokenM
    , putTokenM
    , postTokenM
    , deleteTokenM
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
                                    Token, TokenCreate, TokenResult, Tokens)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureTokenId = Capture "tokenId" Text

type TokenAPI = "tokens" :> (
       ScalewayAuthToken
    :> QueryParam "per_page" PerPage
    :> QueryParam "page" Page
    :> Get '[JSON] Tokens

  :<|> ScalewayAuthToken
    :> CaptureTokenId
    :> Get '[JSON] Token

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] TokenCreate
    :> Post '[JSON] TokenResult

  :<|> ScalewayAuthToken
    :> CaptureTokenId
    :> ReqBody '[JSON] Token
    :> Put '[JSON] TokenResult

  :<|> ScalewayAuthToken
    :> CaptureTokenId
    :> Delete '[JSON] ()
  )

tokenAPI :: Proxy TokenAPI
tokenAPI = Proxy

getTokens_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Tokens
getToken_ :: Maybe XAuthToken -> Text -> ClientM Token
postToken_ :: Maybe XAuthToken -> TokenCreate -> ClientM TokenResult
putToken_ :: Maybe XAuthToken -> Text -> Token -> ClientM TokenResult
deleteToken_ :: Maybe XAuthToken -> Text -> ClientM ()
getTokens_
  :<|> getToken_
  :<|> postToken_
  :<|> putToken_
  :<|> deleteToken_ = client tokenAPI

getTokensM :: Maybe PerPage -> Maybe Page -> ScalewayClient Tokens
getTokensM = scalewayGetListRequest getTokens_

getTokenM :: Text -> ScalewayClient Token
getTokenM = scalewayGetSingleRequest getToken_

postTokenM :: TokenCreate -> ScalewayClient TokenResult
postTokenM = scalewayPostRequest postToken_

putTokenM :: Text -> Token -> ScalewayClient TokenResult
putTokenM = scalewayPutRequest putToken_

deleteTokenM :: Text -> ScalewayClient ()
deleteTokenM = scalewayDeleteRequest deleteToken_
