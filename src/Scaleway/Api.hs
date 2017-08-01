{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.Api where

import           Data.Proxy     (Proxy (..))
import           Data.String    (IsString)
import           Data.Text      (Text, pack)
import           Scaleway.Types (Server, Servers, Volumes)
import           Servant.API    ((:<|>) (..), (:>), Capture, Get, Header, JSON,
                                 QueryParam, ToHttpApiData (toUrlPiece))
import           Servant.Client (ClientM, client)

newtype XAuthToken = XAuthToken Text deriving (Eq, Show, IsString)
newtype PerPage = PerPage Int deriving (Eq, Show, Num)
newtype Page = Page Int deriving (Eq, Show, Num)

instance ToHttpApiData XAuthToken where
  toUrlPiece (XAuthToken t) = t

instance ToHttpApiData PerPage where
  toUrlPiece (PerPage p) = pack $ show p

instance ToHttpApiData Page where
  toUrlPiece (Page p) = pack $ show p

type ScalewayApi = "servers" :> Header "X-Auth-Token" XAuthToken :> QueryParam "per_page" PerPage :> QueryParam "page" Page :> Get '[JSON] Servers
              :<|> "servers" :> Capture "serverId" Text :> Header "X-Auth-Token" XAuthToken :> QueryParam "per_page" PerPage :> QueryParam "page" Page :> Get '[JSON] Server
              :<|> "volumes" :> Header "X-Auth-Token" XAuthToken :> QueryParam "per_page" PerPage :> QueryParam "page" Page :> Get '[JSON] Volumes

api :: Proxy ScalewayApi
api = Proxy

getServersM :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Servers

getServerM :: Text -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Server

getVolumesM :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Volumes

getServersM :<|> getServerM :<|> getVolumesM = client api
