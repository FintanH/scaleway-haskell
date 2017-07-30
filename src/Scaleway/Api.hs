{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Scaleway.Api where

import           Data.Proxy     (Proxy (..))
import           Data.Text      (Text)
import           Scaleway.Types (Server)
import           Servant.API    ((:>), Capture, Get, Header, JSON,
                                 ToHttpApiData (toUrlPiece))
import           Servant.Client (ClientM, client)

newtype XAuthToken = XAuthToken Text deriving (Eq, Show)

instance ToHttpApiData XAuthToken where
  toUrlPiece (XAuthToken t) = t

type ScalewayApi = "servers" :> Header "X-Auth-Token" XAuthToken :> Capture "per_page" Int :> Capture "page" Int :> Get '[JSON] [Server]

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

getServers :: Maybe XAuthToken -> Int -> Int -> ClientM [Server]

getServers = client api
