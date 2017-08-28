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
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAccountClient (..),
                                    ScalewayAuthToken, XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (Token, TokenCreate, TokenId, TokenResult,
                                    Tokens)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, ReqBody)
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

getTokensM :: Maybe PerPage -> Maybe Page -> ScalewayAccountClient Tokens
getTokensM perPage = ScalewayAccount . scalewayGetListRequest getTokens_ perPage

getTokenM :: TokenId -> ScalewayAccountClient Token
getTokenM = ScalewayAccount . scalewayGetSingleRequest getToken_

postTokenM :: TokenCreate -> ScalewayAccountClient TokenResult
postTokenM = ScalewayAccount . scalewayPostRequest postToken_

putTokenM :: TokenId -> Token -> ScalewayAccountClient TokenResult
putTokenM i = ScalewayAccount . scalewayPutRequest putToken_ i

deleteTokenM :: TokenId -> ScalewayAccountClient ()
deleteTokenM = ScalewayAccount . scalewayDeleteRequest deleteToken_
