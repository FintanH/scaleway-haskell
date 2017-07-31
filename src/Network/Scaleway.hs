
module Network.Scaleway where

import           Data.Text               (Text)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Scaleway.Api            (Page, PerPage, XAuthToken, getServerM,
                                          getServersM)
import           Scaleway.Types          (Region, Server, Servers)
import           Servant.Client          (BaseUrl (..), ClientEnv (..),
                                          Scheme (..), ServantError, runClientM)

clientEnv :: Region -> IO ClientEnv
clientEnv region = do
  let host = "cp-" ++ show region ++ ".scaleway.com"
  manager <- newTlsManager
  pure $ ClientEnv manager (BaseUrl Http host 80 "")

getServers :: Region -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError Servers)
getServers region auth perPage page = do
  clientEnv <- clientEnv region
  runClientM (getServersM auth perPage page) clientEnv

getServer :: Region -> Text -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError Server)
getServer region serverId auth perPage page = do
  clientEnv <- clientEnv region
  runClientM (getServerM serverId auth perPage page) clientEnv
