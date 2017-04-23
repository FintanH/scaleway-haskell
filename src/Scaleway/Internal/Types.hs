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
import           Data.Aeson.Types               (Options (..), defaultOptions, Parser)
import qualified Data.HashMap.Strict            as HM
import           Data.Text                      (Text, unpack)
import Data.Monoid ((<>))
import           Data.Time.Clock                (UTCTime)
import           GHC.Generics
import           Scaleway.Internal.Types.Server (BootScript, PublicIp,
                                                 ServerBase (..), ServerName,
                                                 ServerState, parseServerBase)
import           Scaleway.Internal.Types.Volume (VolumeBase (..), VolumeName, parseVolumeBase)
import           Scaleway.Internal.Utility      (jsonCamelCase)

class HasResourceId f a where
  getResourceId :: f -> a

newtype ResourceId a = ResourceId { unResourceId :: a }
  deriving (Show, Eq, Generic)

instance HasResourceId (ResourceId a) a where
  getResourceId = unResourceId

parseResourceId :: (FromJSON a) => Text -> (Value -> Parser (ResourceId a))
parseResourceId key = withObject
                     ("resource id with key: " <> unpack key)
                     (fmap ResourceId . (.: key))

parseResourceIdDefault :: (FromJSON a) => Value -> Parser (ResourceId a)
parseResourceIdDefault = parseResourceId "id"

instance (ToJSON a) => ToJSON (ResourceId a)

type ScalewayId = ResourceId Text

newtype Tag = Tag Text deriving (Show, Eq, ToJSON, FromJSON)

data ServerRef = ServerRef {
    serverId :: ScalewayId
  , name     :: ServerName
} deriving (Show, Eq, Generic)

instance FromJSON ServerRef where
  parseJSON = withObject "server ref" $ \o -> do
    serverId <- parseResourceIdDefault (Object o)
    name <- o .: "name"
    return ServerRef {..}

type ServerPost = ServerBase ScalewayId ScalewayId [Tag]

data ServerGet = ServerGet {
    server     :: ServerBase ScalewayId (Maybe ImageRef) [Tag]
  , serverId   :: ScalewayId
  , bootscript :: Maybe BootScript
  , privateIp  :: Maybe Text
  , publicIp   :: Maybe PublicIp
  , state      :: ServerState
  , volumes    :: HM.HashMap Int VolumeGet
} deriving (Show, Eq, Generic)

instance FromJSON ServerGet where
  parseJSON = withObject "server GET request" $ \o -> do
    let object = (Object o)
    server <- parseServerBase orgParser imageParser tagsParser object
    serverId <- parseResourceIdDefault object
    bootscript <- o .: "bootscript" >>= parseJSON
    privateIp <- o .:? "private_ip"
    publicIp <- o .: "public_ip" >>= parseJSON
    state <- o .: "state" >>= parseJSON
    volumes <- o .: "volumes" >>= parseJSON
    return ServerGet {..}
    where
      orgParser = withObject "organization id" (parseResourceId "organization" . Object)
      imageParser = withObject "server image ref" $ \o -> do
        image <- o .: "image"
        parseJSON (Object image)

      tagsParser = withObject "server tags" (.: "tags")

data ImageRef = ImageRef {
    imageId   :: ScalewayId
  , imageName :: Text
} deriving (Show, Eq, Generic)

instance FromJSON ImageRef where
  parseJSON = withObject "image ref" $ \o -> do
    imageId <- parseResourceIdDefault (Object o)
    imageName <- o .: "name"
    return ImageRef {..}

instance ToJSON ImageRef

type VolumePost = VolumeBase ScalewayId

data VolumeGet = VolumeGet {
    volume           :: VolumeBase ScalewayId
  , volumeId         :: ScalewayId
  , modificationDate :: Maybe UTCTime
  , creationDate     :: Maybe UTCTime
  , exportURI        :: Maybe Text
  , server           :: Maybe ServerRef
} deriving (Show, Eq, Generic)

instance HasResourceId ServerRef Text where
  getResourceId (ServerRef serverId _) = getResourceId serverId

instance HasResourceId ImageRef Text where
  getResourceId (ImageRef imageId _) = getResourceId imageId


instance FromJSON VolumeGet where
  parseJSON = withObject "volume GET response" $ \o -> do
    let object = Object o
    volume <- parseVolumeBase parseOrg object
    volumeId <- parseResourceIdDefault object
    modificationDate <- o .:? "modification_date"
    creationDate <- o .:? "creation_date"
    exportURI <- o .: "export_uri"
    server <- o .: "server" >>= parseJSON
    return VolumeGet {..}
    where
      parseOrg = withObject "volume organization" (parseResourceId "organization" . Object)
