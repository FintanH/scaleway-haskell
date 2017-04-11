{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Server
    ( listServers'
    , listServers
    , retrieveServer'
    , retrieveServer
    ) where

import           Control.Lens
import           Data.Aeson                (Value, eitherDecode, withObject,
                                            (.:))
import           Data.Aeson.Types          (parseEither, parseJSON, toJSON)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, unpack)
import           Network.Wreq              (Response, defaults, deleteWith,
                                            getWith, postWith, responseBody)
import           Scaleway.Internal.Request
import qualified Scaleway.Types.Get        as Get
import           Scaleway.Types.Internal
import qualified Scaleway.Types.Post       as Post
import           Scaleway.Types.Resource   (GetServer, listServer)

listServers' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listServers' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage listServer

listServers :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Server])
listServers headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage listServer

retrieveServer' :: HeaderToken -> Region -> GetServer -> IO (Response ByteString)
retrieveServer' headerToken region server = retrieveResource' headerToken region server

retrieveServer :: HeaderToken -> Region -> GetServer -> IO (Either String Get.Server)
retrieveServer headerToken region server = retrieveResource headerToken region server

createServer :: HeaderToken
             -> Region
             -> Text            -- | Name of the server
             -> OrganizationId  -- | Text ID of the Organization
             -> ImageId         -- | Text ID of the Image
             -> CommercialType  -- | Commercial Type of the machine
             -> [Tag]           -- | List of Tags to assign to the machine
             -> Bool            -- | Enable IPv6
             -> IO (Either String Get.Server)
createServer headerToken region name organization image commercialType tags enableIpv6 = do
  r <- createResource' headerToken region Post.Server{..} "servers"
  return $ parseEither parseServer =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseServer = withObject "server" $ \o -> do
      server <- o .: "server"
      parseJSON server

removeServer :: HeaderToken -> Region -> ServerId -> IO ()
removeServer headerToken region (ServerId serverId) = do
  let url = unUrl (requestUrl region) <> "/servers/" <> (unpack serverId)
      opts = defaults & (scalewayHeader headerToken)
  r <- deleteWith opts url
  print $ "Successfully deleted: " <> serverId
