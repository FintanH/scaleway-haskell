{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.Api where

import           Data.Proxy     (Proxy (..))
import           Data.String    (IsString)
import           Data.Text      (Text, pack)
import           Scaleway.Types (Servers)
import           Servant.API    ((:>), Capture, Get, Header, JSON, QueryParam,
                                 ToHttpApiData (toUrlPiece))
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

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost   :: String   -- ^ host (eg "haskell.org")
  }

api :: Proxy ScalewayApi
api = Proxy

getServersM :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Servers

getServersM = client api
