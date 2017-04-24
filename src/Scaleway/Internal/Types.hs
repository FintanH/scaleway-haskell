{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Scaleway.Internal.Types
    ( ServerGet
    , ServerPost
    ) where

import           Data.Aeson                     (FromJSON, ToJSON,
                                                 genericParseJSON, parseJSON,
                                                 toJSON, withObject, (.:), (.:?), Value (..), withText)
import Control.Applicative ((<|>), liftA2)
import           Data.Aeson.Types               (Options (..), defaultOptions, Parser)
import qualified Data.HashMap.Strict            as HM
import           Data.Text                      (Text, unpack)
import Data.Monoid ((<>))
import           Data.Time.Clock                (UTCTime)
import           GHC.Generics
import           Scaleway.Internal.Types.Image
import           Scaleway.Internal.Types.SecurityGroup
import           Scaleway.Internal.Types.SecurityRule
import           Scaleway.Internal.Types.Server
import           Scaleway.Internal.Types.Volume
import           Scaleway.Internal.Utility      (jsonCamelCase)
import Scaleway.Internal.Types.ResourceId

newtype Tag = Tag Text deriving (Show, Eq, ToJSON, FromJSON)

type ImagePost = ImageBase OrganizationId VolumeRef
data ImageGet = ImageGet {
    image            :: ImageBase OrganizationId VolumeRef
  , creationDate     :: UTCTime
  , extraVolumes     :: Text
  , fromImage        :: Maybe Text
  , fromServer       :: Maybe Text
  , marketplaceKey   :: Maybe Text
  , modificationDate :: UTCTime
  , public           :: Bool
} deriving (Show, Eq)

instance FromJSON ImageGet where
  parseJSON = withObject "image GET response" $ \o -> do
    let object = (Object o)
    image <- parseJSON object -- bit of a cheat here since ImagePost is the same type as ImageBase OrganizationId VolumeRef
    creationDate <- o .: "creation_date"
    extraVolumes <- o .: "extra_volumes"
    fromImage <- o .:? "from_image"
    fromServer <- o .:? "from_server"
    marketplaceKey <- o .:? "marketplace_key"
    modificationDate <- o .: "modification_date"
    public <- o .: "public"
    return ImageGet {..}

instance FromJSON ImagePost where
  parseJSON = parseImageBase parseOrganizationId volRef
    where
      volRef :: Value -> Parser VolumeRef
      volRef = withObject "volume ref" $ \o -> parseJSON =<< o .: "volume"

type SecurityGroupPost = SecurityGroupBase OrganizationId
data SecurityGroupGet = SecurityGroupGet {
    securityGroup         :: SecurityGroupBase OrganizationId
  , securityGroupId       :: SecurityGroupId
  , enableDefaultSecurity :: Bool
  , organizationDefault   :: Bool
  , servers               :: [ServerRef]
} deriving (Show, Eq)

instance FromJSON SecurityGroupGet where
  parseJSON = withObject "security group GET response" $ \o -> do
    securityGroup <- parseSecurityGroupBase parseOrganizationId (Object o)
    securityGroupId <- parseSecurityGroupId (Object o)
    enableDefaultSecurity <- o .: "enable_default_security"
    organizationDefault <- o .: "organization_default"
    servers <- traverse parseJSON =<< o .: "servers"
    return SecurityGroupGet {..}

type ServerPost = ServerBase OrganizationId ImageId [Tag]
data ServerGet = ServerGet {
    server     :: ServerBase OrganizationId (Maybe ImageRef) [Tag]
  , serverId   :: ServerId
  , bootscript :: Maybe BootScript
  , privateIp  :: Maybe Text
  , publicIp   :: Maybe PublicIp
  , state      :: ServerState
  , volumes    :: HM.HashMap Int VolumeGet
} deriving (Show, Eq, Generic)

instance FromJSON ServerGet where
  parseJSON = withObject "server GET request" $ \o -> do
    let object = (Object o)
    server <- parseServerBase parseOrganizationId imageParser tagsParser object
    serverId <- parseServerId object
    bootscript <- o .: "bootscript" >>= parseJSON
    privateIp <- o .:? "private_ip"
    publicIp <- o .: "public_ip" >>= parseJSON
    state <- o .: "state" >>= parseJSON
    volumes <- o .: "volumes" >>= parseJSON
    return ServerGet {..}
    where
      tagsParser = withObject "server tags" (.: "tags")
      imageParser = withObject "image ref" $ \o -> do
        image <- o .: "image"
        parseJSON image

type VolumePost = VolumeBase OrganizationId
data VolumeGet = VolumeGet {
    volume           :: VolumeBase OrganizationId
  , volumeId         :: VolumeId
  , modificationDate :: Maybe UTCTime
  , creationDate     :: Maybe UTCTime
  , exportURI        :: Maybe Text
  , server           :: Maybe ServerRef
} deriving (Show, Eq, Generic)

instance FromJSON VolumeGet where
  parseJSON = withObject "volume GET response" $ \o -> do
    let object = Object o
    volume <- parseVolumeBase parseOrganizationId object
    volumeId <- parseVolumeId object
    modificationDate <- o .:? "modification_date"
    creationDate <- o .:? "creation_date"
    exportURI <- o .: "export_uri"
    server <- o .: "server" >>= parseJSON
    return VolumeGet {..}
