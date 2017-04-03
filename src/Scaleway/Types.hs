{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Scaleway.Types
    (
    ) where

import           Data.Aeson
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics

-- | Server Types
newtype ServerId = ServerId Text deriving (Show, Eq, Generic)

data ServerState = Running
                 | Stopped
                 | Booted
                 deriving (Eq, Generic)

data Server = Server {
    serverID        :: ServerId
  , severName       :: Text
  , image           :: ImageRef
  , bootscript      :: Maybe Text
  , dynamicPublicIP :: Bool
  , organization    :: OrganizationId
  , privateIP       :: Maybe Text
  , publicIP        :: Maybe Text
  , state           :: ServerState
  , tags            :: [Text]
  , volumes         :: [Volume]
} deriving (Show, Eq, Generic)

-- | Image Types
newtype ImageId = ImageId Text deriving (Show, Eq, Generic)

data ImageRef = ImageRef {
    imageID   :: ImageId
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
  , rootVolume        :: VolumRef
} deriving (Show, Eq, Generic)


-- | Organization Types
newtype OrganizationId = OrganizationId Text deriving (Show, Eq, Generic)

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
newtype UserId = UserId Text deriving (Show, Eq, Generic)

data Role = Manager deriving (Show, Eq, Generic)

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
newtype VolumeId = VolumeId Text deriving (Show, Eq, Generic)

data VolumRef = VolumRef {
    volumeId   :: VolumeId
  , volumeName :: Text
} deriving (Show, Eq, Generic)

data Volume = Volume {
    volumeId     :: VolumeId
  , volumeName   :: Text
  , exportURI    :: Maybe Bool
  , organization :: OrganizationId
  , server       :: Maybe Server
  , size         :: Int
  , volumeType   :: Text
} deriving (Show, Eq, Generic)

-- | Instances
instance FromJSON ServerId
instance ToJSON ServerId

instance FromJSON Server
instance ToJSON Server

instance Show ServerState where
  show Running = "running"
  show Stopped = "stopped"
  show Booted = "booted"

instance FromJSON ServerState
instance ToJSON ServerState

instance FromJSON ImageId
instance ToJSON ImageId

instance FromJSON ImageRef
instance ToJSON ImageRef

instance FromJSON Image
instance ToJSON Image

instance FromJSON OrganizationId
instance ToJSON OrganizationId

instance FromJSON Organization
instance ToJSON Organization

instance FromJSON UserId
instance ToJSON UserId

instance FromJSON Role
instance ToJSON Role

instance FromJSON User
instance ToJSON User

instance FromJSON VolumeId
instance ToJSON VolumeId

instance FromJSON VolumRef
instance ToJSON VolumRef

instance FromJSON Volume
instance ToJSON Volume
