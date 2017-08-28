{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.User
    ( UserAPI
    , getUserM
    ) where

import           Data.Proxy        (Proxy (..))
import           Scaleway.API.Core (ScalewayAccountClient (..),
                                    ScalewayAuthToken, XAuthToken,
                                    scalewayGetSingleRequest)
import           Scaleway.Types    (UserId, UserResult)
import           Servant.API       ((:<|>) (..), (:>), Capture, Get, JSON,
                                    QueryParam)
import           Servant.Client    (ClientM, client)

type CaptureUserId = Capture "userId" UserId

type UserAPI =
       "users" :> ScalewayAuthToken
               :> CaptureUserId
               :> Get '[JSON] UserResult

userAPI :: Proxy UserAPI
userAPI = Proxy

getUser_ :: Maybe XAuthToken -> UserId -> ClientM UserResult
getUser_ = client userAPI

getUserM :: UserId -> ScalewayAccountClient UserResult
getUserM = ScalewayAccount . scalewayGetSingleRequest getUser_
