{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scaleway.Network.Servers where

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

listServers' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listServers' headerToken region pageNumber nPerPage = listResource' headerToken region pageNumber nPerPage "servers"

listServers :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Get.Server])
listServers headerToken region pageNumber nPerPage = listResource headerToken region pageNumber nPerPage "servers"

retrieveServer' :: HeaderToken -> Region -> ServerId -> IO (Response ByteString)
retrieveServer' headerToken region serverId = retrieveResource' headerToken region "servers" serverId

retrieveServer :: HeaderToken -> Region -> ServerId -> IO (Either String Get.Server)
retrieveServer headerToken region serverId = do
  r <- retrieveServer' headerToken region serverId
  return $ parseEither parseServer =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseServer = withObject "server" $ \o -> do
      server <- o .: "server"
      parseJSON server

createServer' :: HeaderToken
              -> Region
              -> Text            -- | Name of the server
              -> OrganizationId  -- | Text ID of the Organization
              -> ImageId         -- | Text ID of the Image
              -> CommercialType  -- | Commercial Type of the machine
              -> [Tag]           -- | List of Tags to assign to the machine
              -> Bool            -- | Enable IPv6
              -> IO (Response ByteString)
createServer' headerToken region name organization image commercialType tags enableIpv6 = do
  let serverJson = toJSON $ Post.Server{..}
      url = unUrl (requestUrl region) <> "/servers"
      opts = defaults & (scalewayHeader headerToken)
  print $ "POSTing: " <> show serverJson
  postWith opts url serverJson

createServer :: HeaderToken
             -> Region
             -> Text            -- | Name of the server
             -> OrganizationId  -- | Text ID of the Organization
             -> ImageId         -- | Text ID of the Image
             -> CommercialType  -- | Commercial Type of the machine
             -> [Tag]           -- | List of Tags to assign to the machine
             -> Bool            -- | Enable IPv6
             -> IO (Either String Get.Server)
createServer headerToken region serverName organizationId image commercialType tags enableIpv6 = do
  r <- createServer' headerToken region serverName organizationId image commercialType tags enableIpv6
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
