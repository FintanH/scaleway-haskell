
module Network.Scaleway where

import           Data.Text               (Text)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Scaleway.API            (getServerM, getServersM)
import           Scaleway.API.Core       (Page, PerPage, XAuthToken)
import           Scaleway.Types          (Images, PublicIps, Region, Server,
                                          Servers, Volumes)
import           Servant.Client          (BaseUrl (..), ClientEnv (..), ClientM,
                                          Scheme (..), ServantError, runClientM)


clientEnv :: Region -> IO ClientEnv
clientEnv region = do
  let host = "cp-" ++ show region ++ ".scaleway.com"
  manager <- newTlsManager
  pure $ ClientEnv manager (BaseUrl Http host 80 "")


getResources :: Region
             -> ClientM a
             -> IO (Either ServantError a)
getResources region requestM = do
  env <- clientEnv region
  runClientM requestM env


getServers :: Region -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError Servers)
getServers region auth perPage page =
  getResources region (getServersM auth perPage page)


getServer :: Region -> Text -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError Server)
getServer region serverId auth perPage page = do
  env <- clientEnv region
  runClientM (getServerM auth serverId perPage page) env


-- getVolumes :: Region -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError Volumes)
-- getVolumes region auth perPage page =
--   getResources region (getVolumesM auth perPage page)
--
--
-- getImages :: Region -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError Images)
-- getImages region auth perPage page =
--   getResources region (getImagesM auth perPage page)
--
-- getIps :: Region -> Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> IO (Either ServantError PublicIps)
-- getIps region auth perPage page =
--   getResources region (getIpsM auth perPage page)
