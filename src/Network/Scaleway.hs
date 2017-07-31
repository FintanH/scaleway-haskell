
module Network.Scaleway where

import           Network.HTTP.Client.TLS (newTlsManager)
import           Scaleway.Api            (Page, PerPage, XAuthToken,
                                          getServersM)
import           Scaleway.Types          (Region, Servers)
import           Servant.Client          (BaseUrl (..), ClientEnv (..),
                                          Scheme (..), ServantError, runClientM)


getServers :: Region -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError Servers)
getServers region auth perPage page = do
  let host = "cp-" ++ show region ++ ".scaleway.com"
  manager <- newTlsManager
  runClientM (getServersM auth perPage page) (ClientEnv manager (BaseUrl Http host 80 ""))
