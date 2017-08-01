{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Scaleway.Types where

import           Data.Aeson        (FromJSON (..), ToJSON, Value (Array),
                                    genericParseJSON, withObject, withText,
                                    (.:))
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, fieldLabelModifier)
import           Data.Char         (toLower)
import           Data.Text         (Text, unpack)
import           Data.Time         (UTCTime)
import           GHC.Generics


serverPrefix :: String
serverPrefix = "server"

data Server = Server {
    serverId             :: Text
  , serverName           :: Text
  , serverOrganization   :: Text
  , serverImage          :: Image
  , serverCommercialType :: CommercialType
  , serverTags           :: [Text]
  , serverEnableIpv6     :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON Server where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverPrefix) }


newtype Servers = Servers { servers :: [Server] } deriving (Show, Eq, Generic)

instance FromJSON Servers


serverRefPrefix :: String
serverRefPrefix = "serverRef"

data ServerRef = ServerRef {
    serverRefId   :: Text
  , serverRefName :: Text
} deriving (Show, Eq, Generic)

instance FromJSON ServerRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverRefPrefix) }

-------------------------------------------------------------------------------


data CommercialType = VC1S
                    | VC1M
                    | VC1L
                    | C1
                    | C2S
                    | C2M
                    | C2L
                    | ARM64_128GB
                    deriving (Show, Eq, Generic)

instance FromJSON CommercialType where
  parseJSON = withText "commercial_type" $ \t ->
    case t of
      "VC1S"        -> pure VC1S
      "VC1M"        -> pure VC1M
      "VC1L"        -> pure VC1L
      "C1"          -> pure C1
      "C2S"         -> pure C2S
      "C2M"         -> pure C2M
      "C2L"         -> pure C2L
      "ARM64-128GB" -> pure ARM64_128GB
      _             -> fail $ "Unknown commercial_type: " ++ (unpack t)
instance ToJSON CommercialType


-------------------------------------------------------------------------------

organizationPrefix :: String
organizationPrefix = "organization"

data Organization = Organization {
    organizationId    :: Text
  , organizationName  :: Text
  , organizationUsers :: [User]
} deriving (Show, Eq, Generic)

instance FromJSON Organization where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length organizationPrefix) }


-------------------------------------------------------------------------------


imagePrefix :: String
imagePrefix = "image"

data Image = Image {
    imageId               :: Text
  , imageName             :: Text
  , imageOrganization     :: Text
  , imageRootVolume       :: VolumeRef
  , imageArch             :: Text
  , imageCreationDate     :: UTCTime
  , imageExtraVolumes     :: Text
  , imageFromImage        :: Maybe Text
  , imageFromServer       :: Maybe Text
  , imageMarketplaceKey   :: Maybe Text
  , imageModificationDate :: UTCTime
  , imagePublic           :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Image where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length imagePrefix) }


-------------------------------------------------------------------------------


userPrefix :: String
userPrefix = "user"

data User = User {
    userId            :: Text
  , userEmail         :: Text
  , userFirstname     :: Text
  , userLastname      :: Text
  , userFullname      :: Text
  , userOrganizations :: Maybe [Text]
  , userRoles         :: Maybe [Role]
  , userSshPublicKeys :: Maybe [Text]
} deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length userPrefix) }


-------------------------------------------------------------------------------


data RoleType = Manager
  deriving (Show, Eq, Generic)

instance FromJSON RoleType
instance ToJSON RoleType

rolePrefix :: String
rolePrefix = "role"

data Role = Role {
    roleOrganization :: Maybe Text
  , role             :: Maybe RoleType
} deriving (Show, Eq, Generic)

instance FromJSON Role where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }
    where
      modify "role" = "role"
      modify s      = snakeCase . drop (length rolePrefix) $ s


-------------------------------------------------------------------------------

volumePrefix :: String
volumePrefix = "volume"

data Volume = Volume {
      volumeId               :: Text
    , volumeName             :: Text
    , volumeOrganization     :: Text
    , volumeSize             :: Int
    , volumeType             :: Text
    , volumeModificationDate :: Maybe UTCTime
    , volumeCreationDate     :: Maybe UTCTime
    , volumeExportURI        :: Maybe Text
    , volumeServer           :: Maybe ServerRef
} deriving (Show, Eq, Generic)

instance FromJSON Volume where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }
    where
      modify "volumeType" = "volume_type"
      modify s            = snakeCase . drop (length volumePrefix) $ s


newtype Volumes = Volumes { volumes :: [Volume] } deriving (Show, Eq, Generic)

instance FromJSON Volumes


volumeRefPrefix :: String
volumeRefPrefix = "volumeRef"

data VolumeRef = VolumeRef {
    volumeRefName :: Text
  , volumeRefId   :: Text
} deriving (Show, Eq, Generic)

instance FromJSON VolumeRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length volumeRefPrefix) }

instance ToJSON VolumeRef


-------------------------------------------------------------------------------

data Region =
    Paris
  | Amsterdam
  deriving (Eq)

instance Show Region where
  show Paris     = "par1"
  show Amsterdam = "ams1"
