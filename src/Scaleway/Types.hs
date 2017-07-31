{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scaleway.Types where

import           Data.Aeson    (FromJSON (..), ToJSON, genericParseJSON)
import           Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import           Data.Char     (toLower)
import           Data.Text     (Text)
import           Data.Time     (UTCTime)
import           GHC.Generics


data Server = Server {
    serverId             :: Text
  , serverName           :: Text
  , serverOrganization   :: Organization
  , serverImage          :: Image
  , serverCommercialType :: CommercialType
  , serverTags           :: [Text]
  , serverEnableIpv6     :: Maybe Bool
} deriving (Show, Eq, Generic)

newtype Servers = Servers { servers :: [Server] } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromJSON Server where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 6 }

instance ToJSON Server

-------------------------------------------------------------------------------


data CommercialType = VC1S
                    | VC1M
                    | VC1L
                    | C1
                    | C2S
                    | C2M
                    | C2L
                    | ARM64
                    deriving (Show, Eq, Generic)

instance FromJSON CommercialType
instance ToJSON CommercialType


-------------------------------------------------------------------------------


data Organization = Organization {
    organizationId    :: Text
  , organizationName  :: Text
  , organizationUsers :: [User]
} deriving (Show, Eq, Generic)

instance FromJSON Organization where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 12 }

instance ToJSON Organization

-------------------------------------------------------------------------------

data Image = Image {
    imageId               :: Text
  , imageName             :: Text
  , imageOrganization     :: Text
  , imageRootVolume       :: VolumeRef
  , imageArchitecture     :: Text
  , imageCreationDate     :: UTCTime
  , imageExtraVolumes     :: Text
  , imageFromImage        :: Maybe Text
  , imageFromServer       :: Maybe Text
  , imageMarketplaceKey   :: Maybe Text
  , imageModificationDate :: UTCTime
  , imagePublic           :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Image where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 5 }

instance ToJSON Image

-------------------------------------------------------------------------------


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

instance FromJSON User
instance ToJSON User


-------------------------------------------------------------------------------
data RoleType = Manager
  deriving (Show, Eq, Generic)

instance FromJSON RoleType
instance ToJSON RoleType


data Role = Role {
    organization :: Maybe Text
  , role         :: Maybe RoleType
} deriving (Show, Eq, Generic)

instance FromJSON Role
instance ToJSON Role


-------------------------------------------------------------------------------


data VolumeRef = VolumeRef {
    volumeRefName :: Text
  , volumeRefId   :: Text
} deriving (Show, Eq, Generic)

instance FromJSON VolumeRef
instance ToJSON VolumeRef


---

data Region =
    Paris
  | Amsterdam
  deriving (Eq)

instance Show Region where
  show Paris     = "par1"
  show Amsterdam = "ams1"
