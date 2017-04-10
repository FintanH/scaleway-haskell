{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Scaleway.Types.Get
    ( Server(..)
    , ServerId(..)
    ) where

import           Data.Aeson
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Types          (Options (..), defaultOptions)
import qualified Data.HashMap.Strict       as HM
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)
import           GHC.Generics
import           Scaleway.Internal.Utility (jsonCamelCase)
import           Scaleway.Types.Internal

-- | Server Types
data ServerRef = ServerRef {
    serverId   :: ServerId
  , serverName :: Text
} deriving (Show, Eq, Generic)

data Server = Server {
    serverId       :: ServerId
  , serverName     :: Text
  , image          :: ImageRef
  , bootscript     :: Maybe BootScript
  , organization   :: OrganizationId
  , privateIp      :: Maybe Text
  , publicIp       :: Maybe PublicIp
  , state          :: ServerState
  , tags           :: [Tag]
  , volumes        :: HM.HashMap Text Volume
  , commercialType :: CommercialType
} deriving (Show, Eq, Generic)

-- | Image Types
data ImageRef = ImageRef {
    imageId   :: ImageId
  , imageName :: Text
} deriving (Show, Eq, Generic)

data Image = Image {
    imageId           :: ImageId
  , imageName         :: Text
  , architecture      :: Text
  , creationDate      :: UTCTime
  , extraVolumes      :: [Volume]
  , fromImage         :: Maybe Text
  , fromServer        :: Maybe Text
  , marketplaceKey    :: Maybe Text
  , modificationDate  :: UTCTime
  , organizationImage :: OrganizationId
  , public            :: Bool
  , rootVolume        :: VolumeRef
} deriving (Show, Eq, Generic)


-- | Organization Types
data OrganizationRef = OrganizationRef {
    organizationId   :: OrganizationId
  , organizationName :: Text
}

data Organization = Organization {
    organizationId   :: OrganizationId
  , organizationName :: Text
  , users            :: [User]
} deriving (Show, Eq, Generic)


-- | User Types
data User = User {
    userId        :: UserId
  , email         :: Text
  , firstName     :: Text
  , lastName      :: Text
  , fullName      :: Text
  , organizations :: Maybe [Organization]
  , roles         :: Maybe [Role]
  , sshPublicKeys :: Maybe [Text]
} deriving (Show, Eq, Generic)


-- | Volume Types
data VolumeRef = VolumeRef {
    volumeId   :: VolumeId
  , volumeName :: Text
} deriving (Show, Eq, Generic)

data Volume = Volume {
    volumeId         :: VolumeId
  , volumeName       :: Text
  , modificationDate :: Maybe UTCTime
  , creationDate     :: UTCTime
  , exportURI        :: Maybe Text
  , organization     :: OrganizationId
  , server           :: Maybe ServerRef
  , size             :: Int
  , volumeType       :: Text
} deriving (Show, Eq, Generic)

-- | Snapshot Types
data Snapshot = Snapshot {
    snapshotId   :: SnapshotId
  , snapshotName :: Text
  , baseVolume   :: VolumeRef
  , creationDate :: UTCTime
  , organization :: OrganizationId
  , size         :: Int
  , state        :: SnapshotState
  , volumeType   :: Text
} deriving (Show, Eq, Generic)


-- | IP Types
data IP = IP {
    ipId         :: IpId
  , address      :: Text
  , organization :: OrganizationId
  , server       :: Maybe Server
} deriving (Show, Eq, Generic)


-- | Security Group Types
data SecurityGroup = SecurityGroup {
    securityGroupId       :: SecurityGroupId
  , name                  :: Text
  , description           :: Text
  , enableDefaultSecurity :: Bool
  , organization          :: OrganizationId
  , organizationDefault   :: Bool
  , servers               :: [ServerRef]
} deriving (Show, Eq, Generic)


-- | Security Rule
data SecurityRule = SecurityRule {
    securityRuleId :: SecurityRuleId
  , ipRange        :: Text
  , direction      :: Direction
  , protocol       :: Protocol
  , destPortFrom   :: Maybe Int
  , destPortTo     :: Maybe Int
  , action         :: Text
  , position       :: Int
  , editable       :: Maybe Bool
} deriving (Show, Eq, Generic)


-- | Token
data Token = Token {
    tokenId           :: TokenId
  , creationDate      :: UTCTime
  , expires           :: Maybe UTCTime
  , inheritsUserPerms :: Bool
  , permissions       :: [Text]
  , roles             :: (Organization, Role)
  , userId            :: UserId
} deriving (Show, Eq, Generic)

-- | Instances
instance FromJSON Server where
  parseJSON = genericParseJSON opts . jsonCamelCase
    where
      opts = defaultOptions { fieldLabelModifier = modifyNames }
      modifyNames "serverId"   = "id"
      modifyNames "serverName" = "name"
      modifyNames x            = x

instance ToJSON Server

instance FromJSON ServerRef where
  parseJSON = genericParseJSON opts . jsonCamelCase
    where
      opts = defaultOptions { fieldLabelModifier = modifyNames }
      modifyNames "serverId"   = "id"
      modifyNames "serverName" = "name"
      modifyNames x            = x

instance ToJSON ServerRef

instance FromJSON ImageRef where
  parseJSON = genericParseJSON opts . jsonCamelCase
    where
      opts = defaultOptions { fieldLabelModifier = modifyNames }
      modifyNames "imageId"   = "id"
      modifyNames "imageName" = "name"
      modifyNames x           = x

instance ToJSON ImageRef

instance FromJSON Image
instance ToJSON Image

instance FromJSON Organization
instance ToJSON Organization

instance FromJSON User
instance ToJSON User

instance FromJSON VolumeRef
instance ToJSON VolumeRef

instance FromJSON Volume where
  parseJSON = genericParseJSON opts . jsonCamelCase
    where
      opts = defaultOptions { fieldLabelModifier = modifyNames }
      modifyNames "volumeId"   = "id"
      modifyNames "volumeName" = "name"
      modifyNames "exportURI"  = "exportUri"
      modifyNames x            = x

instance ToJSON Volume
