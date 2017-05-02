
{-# LANGUAGE RecordWildCards #-}

module Scaleway.Network.Server
    ( listServers'
    , listServers
    , retrieveServer'
    , retrieveServer
    ) where

import           Control.Lens
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader)
import           Data.Aeson
import           Data.Aeson.Types              (parseEither)
import           Data.ByteString.Lazy          (ByteString)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text, unpack)
import           Network.Wreq
import           Scaleway.Internal.Request     (HeaderToken, Page, PerPage,
                                                createResource', listResource,
                                                listResource', requestUrl,
                                                retrieveResource,
                                                retrieveResource',
                                                removeResource,
                                                scalewayHeader, unUrl)
import Scaleway.Internal.Types (Server, mkServerData, CommercialType, ScalewayEnv, GetServer, listServer,ImageId,
                                                OrganizationId, Region,
                                                ServerId (..), Tag)

listServers' :: (MonadReader ScalewayEnv m, MonadIO m)
             => Page -> PerPage -> m (Response ByteString)
listServers' pageNumber nPerPage = listResource' pageNumber nPerPage listServer

listServers :: (MonadReader ScalewayEnv m, MonadIO m)
            => Page -> PerPage -> m (Either String [Server])
listServers pageNumber nPerPage = listResource pageNumber nPerPage listServer

retrieveServer' :: (MonadReader ScalewayEnv m, MonadIO m)
                => GetServer -> m (Response ByteString)
retrieveServer' = retrieveResource'

retrieveServer :: (MonadReader ScalewayEnv m, MonadIO m)
               => GetServer -> m (Either String Server)
retrieveServer = retrieveResource

createServer :: (MonadReader ScalewayEnv m, MonadIO m)
             => Text            -- | Name of the server
             -> OrganizationId  -- | Text ID of the Organization
             -> ImageId         -- | Text ID of the Image
             -> CommercialType  -- | Commercial Type of the machine
             -> [Tag]           -- | List of Tags to assign to the machine
             -> Maybe Bool            -- | Enable IPv6
             -> m (Either String Server)
createServer name organization image commercialType tags enableIpv6 = do
  r <- createResource' (mkServerData name organization image commercialType tags enableIpv6) "servers"
  return $ parseEither parseServer =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseServer = withObject "server" $ \o -> do
      server <- o .: "server"
      parseJSON server

removeServer :: (MonadReader ScalewayEnv m, MonadIO m)
             => ServerId -> m ()
removeServer serverId = removeResource serverId "servers"
