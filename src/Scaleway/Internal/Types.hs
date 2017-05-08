{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Scaleway.Internal.Types
    ( Image (..)
    , ImageData (..)
    , Organization (..)
    , SecurityGroup (..)
    , SecurityGroupData (..)
    , SecurityRule (..)
    , SecurityRuleData (..)
    , Server (..)
    , ServerData (..)
    , ServerState (..)
    , PublicIp (..)
    , mkServerData
    , Snapshot (..)
    , SnapshotData (..)
    , Token (..)
    , TokenData (..)
    , User (..)
    , Volume (..)
    , VolumeData (..)
    , Tag (..)
    , RoleType (..)
    , Role (..)
    , CommercialType (..)
    , Region (..)
    , ScalewayEnv (..)
    , HeaderToken
    , PerPage
    , ScalewayPaginate (..)
    , ScalewayRequest (..)
    , module Scaleway.Internal.Types.Get
    , module Scaleway.Internal.Types.ResourceId
    ) where

import           Data.Aeson                     (FromJSON, ToJSON,
                                                 genericParseJSON, parseJSON,
                                                 toJSON, withObject, (.:), (.:?), Value (..), withText, (.=), object)
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
import           Scaleway.Internal.Types.Snapshot
import           Scaleway.Internal.Utility      (jsonCamelCase)
import Scaleway.Internal.Types.ResourceId
import Scaleway.Internal.Types.Get

import qualified Data.ByteString as BS

type HeaderToken = BS.ByteString
type Page = Text
type PerPage = Text

data ScalewayEnv = ScalewayEnv {
    authToken :: HeaderToken
  , region    :: Region
} deriving (Show, Eq)

data ScalewayPaginate = ScalewayPaginate {
    perPage :: PerPage
  , pageNumber :: Page
} deriving (Show, Eq)

data ScalewayRequest = ScalewayRequest {
    env :: ScalewayEnv
  , paginate :: ScalewayPaginate
} deriving (Show, Eq)

data Region =
    Paris
  | Amsterdam
  deriving (Eq)

instance Show Region where
  show r =
    case r of
      Paris -> "par1"
      Amsterdam -> "ams1"

newtype Tag = Tag Text deriving (Show, Eq, ToJSON, FromJSON)

data RoleType = Manager
  deriving (Show, Eq)

instance FromJSON RoleType where
  parseJSON = withText "role type" $ \t -> do
    case t of
      "manager" -> pure Manager
      _         -> fail (unpack $ "Unknown role type: " <> t)

data Role = Role {
    organization :: Maybe OrganizationId
  , role :: Maybe RoleType
} deriving (Show, Eq)

instance FromJSON Role where
  parseJSON = withObject "role" $ \o -> do
    organization <- (fmap ResourceId) <$> o .:? "organization"
    role <- (o .:? "role") >>= traverse parseJSON
    pure Role {..}


type ImageData = ImageBase OrganizationId VolumeRef
data Image = Image {
    image            :: ImageBase OrganizationId VolumeRef
  , creationDate     :: UTCTime
  , extraVolumes     :: Text
  , fromImage        :: Maybe Text
  , fromServer       :: Maybe Text
  , marketplaceKey   :: Maybe Text
  , modificationDate :: UTCTime
  , public           :: Bool
} deriving (Show, Eq)

instance FromJSON Image where
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
    pure Image {..}

instance FromJSON ImageData where
  parseJSON = parseImageBase parseOrganizationId volRef
    where
      volRef :: Value -> Parser VolumeRef
      volRef = withObject "volume ref" $ \o -> parseJSON =<< o .: "volume"


data Organization = Organization {
    organizationId :: OrganizationId
  , name :: Text
  , users :: [User]
}

instance FromJSON Organization where
  parseJSON = withObject "organization" $ \o -> do
    organizationId <- parseOrganizationId (Object o)
    name <- o .: "name"
    users <- o .: "users" >>= traverse parseJSON
    pure Organization {..}


type SecurityGroupData = SecurityGroupBase OrganizationId
data SecurityGroup = SecurityGroup {
    securityGroup         :: SecurityGroupBase OrganizationId
  , securityGroupId       :: SecurityGroupId
  , enableDefaultSecurity :: Bool
  , organizationDefault   :: Bool
  , servers               :: [ServerRef]
} deriving (Show, Eq)

instance FromJSON SecurityGroup where
  parseJSON = withObject "security group GET response" $ \o -> do
    securityGroup <- parseSecurityGroupBase parseOrganizationId (Object o)
    securityGroupId <- parseSecurityGroupId (Object o)
    enableDefaultSecurity <- o .: "enable_default_security"
    organizationDefault <- o .: "organization_default"
    servers <- traverse parseJSON =<< o .: "servers"
    pure SecurityGroup {..}


type ServerData = ServerBase OrganizationId ImageId [Tag]
data Server = Server {
    server     :: ServerBase OrganizationId (Maybe ImageRef) [Tag]
  , serverId   :: ServerId
  , bootscript :: Maybe BootScript
  , privateIp  :: Maybe Text
  , publicIp   :: Maybe PublicIp
  , state      :: ServerState
  , volumes    :: HM.HashMap Int Volume
} deriving (Show, Eq, Generic)

mkServerData = ServerBase

instance FromJSON Server where
  parseJSON = withObject "server GET request" $ \o -> do
    let object = (Object o)
    server <- parseServerBase parseOrganizationId imageParser tagsParser object
    serverId <- parseServerId object
    bootscript <- o .: "bootscript" >>= parseJSON
    privateIp <- o .:? "private_ip"
    publicIp <- o .: "public_ip" >>= parseJSON
    state <- o .: "state" >>= parseJSON
    volumes <- o .: "volumes" >>= parseJSON
    pure Server {..}
    where
      tagsParser = withObject "server tags" (.: "tags")
      imageParser = withObject "image ref" $ \o -> do
        image <- o .: "image"
        parseJSON image

        -- name           :: ServerName
        -- , organization   :: org
        -- , image          :: image
        -- , commercialType :: CommercialType
        -- , tags           :: tags
        -- , enableIpv6     :: Maybe Bool
instance ToJSON ServerData where
  toJSON ServerBase {..} = object [
      "organization" .= toJSON (unResourceId organization),
      "image" .= image,
      "commercial_type" .= commercialType,
      "tags" .= tags,
      "enable_ipv6" .= enableIpv6 ]

type SnapshotData = SnapshotBase OrganizationId VolumeId
data Snapshot = Snapshot {
    snapshot :: SnapshotBase OrganizationId VolumeRef
  , snapshotId :: SnapshotId
  , creationDate :: UTCTime
  , size :: Int
  , state :: SnapshotState
  , volumeType :: Text
} deriving (Show, Eq)

instance FromJSON Snapshot where
  parseJSON = withObject "snapshot GET request" $ \o -> do
    snapshot <- parseSnapshotBase parseOrganizationId volumeParser (Object o)
    snapshotId <- parseSnapshotId (Object o)
    creationDate <- o .: "creation_date"
    size <- o .: "size"
    state <- o .: "state" >>= parseJSON
    volumeType <- o .: "volume_type"
    pure Snapshot {..}
    where
      volumeParser = withObject "volume ref" $ \o -> do
        volume <- o .: "base_volume"
        parseJSON volume


data TokenData = TokenData {
    email :: Text
  , password :: Text
  , expires :: Bool
} deriving (Show, Eq)

data Token = Token {
    tokenId :: TokenId
  , creationDate :: UTCTime
  , expires :: Bool
  , inheritsUserPerms :: Bool
  , permissions :: [Text]
  , roles :: Role
} deriving (Show, Eq)

instance ToJSON TokenData where
  toJSON = error "TODO: TokenData"

instance FromJSON Token where
  parseJSON = withObject "token GET request" $ \o -> do
    tokenId <- parseTokenId (Object o)
    creationDate <- o .: "creation_date"
    expires <- o .: "expires"
    inheritsUserPerms <- o .: "inherits_user_perms"
    permissions <- o .: "permissions"
    roles <- parseJSON (Object o)
    pure Token {..}


data User = User {
    userId :: UserId
  , email :: Text
  , firstname :: Text
  , lastname :: Text
  , fullname :: Text
  , organizations :: Maybe [OrganizationId]
  , roles :: Maybe [Role]
  , sshPublicKeys :: Maybe [Text]
} deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    userId <- parseUserId (Object o)
    email <- o .: "email"
    firstname <- o .: "firstname"
    lastname <- o .: "lastname"
    fullname <- o .: "fullname"
    organizations <- fmap (map ResourceId) <$> o .:? "organizations"
    roles <- o .:? "roles" >>= traverse parseJSON
    sshPublicKeys <- o .:? "ssh_public_keys"
    pure User {..}


type VolumeData = VolumeBase OrganizationId
data Volume = Volume {
    volume           :: VolumeBase OrganizationId
  , volumeId         :: VolumeId
  , modificationDate :: Maybe UTCTime
  , creationDate     :: Maybe UTCTime
  , exportURI        :: Maybe Text
  , server           :: Maybe ServerRef
} deriving (Show, Eq, Generic)

instance FromJSON Volume where
  parseJSON = withObject "volume GET response" $ \o -> do
    let object = Object o
    volume <- parseVolumeBase parseOrganizationId object
    volumeId <- parseVolumeId object
    modificationDate <- o .:? "modification_date"
    creationDate <- o .:? "creation_date"
    exportURI <- o .: "export_uri"
    server <- o .: "server" >>= parseJSON
    pure Volume {..}
