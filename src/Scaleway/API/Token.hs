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
                                    ScalewayClient, XAuthToken, ParamPerPage, ParamPage,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions,
                                    Token, TokenCreate, TokenResult, Tokens, TokenId)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureTokenId = Capture "tokenId" TokenId

type TokenAPI = "tokens" :> (
       ScalewayAuthToken
    :> ParamPerPage
    :> ParamPage
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
getToken_ :: Maybe XAuthToken -> TokenId -> ClientM Token
postToken_ :: Maybe XAuthToken -> TokenCreate -> ClientM TokenResult
putToken_ :: Maybe XAuthToken -> TokenId -> Token -> ClientM TokenResult
deleteToken_ :: Maybe XAuthToken -> TokenId -> ClientM ()
getTokens_
  :<|> getToken_
  :<|> postToken_
  :<|> putToken_
  :<|> deleteToken_ = client tokenAPI

getTokensM :: Maybe PerPage -> Maybe Page -> ScalewayClient Tokens
getTokensM = scalewayGetListRequest getTokens_

getTokenM :: TokenId -> ScalewayClient Token
getTokenM = scalewayGetSingleRequest getToken_

postTokenM :: TokenCreate -> ScalewayClient TokenResult
postTokenM = scalewayPostRequest postToken_

putTokenM :: TokenId -> Token -> ScalewayClient TokenResult
putTokenM = scalewayPutRequest putToken_

deleteTokenM :: TokenId -> ScalewayClient ()
deleteTokenM = scalewayDeleteRequest deleteToken_
