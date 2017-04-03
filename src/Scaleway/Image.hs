{-# LANGUAGE DeriveGeneric #-}

module Scaleway.Image
    ( Image
    , ImageId
    ) where

import           Data.Aeson
import           Data.Text             (Text)
import           Data.Time.Clock       (UTCTime)
import           GHC.Generics
import           Scaleway.Organization (OrganizationId)
import           Scaleway.Volume       (Volume, VolumeId)

newtype ImageId = ImageId Text deriving (Show, Eq, Generic)

data VolumRef = VolumRef {
    volumeId   :: VolumeId
  , volumeName :: Text
} deriving (Show, Eq, Generic)

data Image = Image {
    imageId          :: ImageId
  , imageName        :: Text
  , architecture     :: Text
  , creationDate     :: UTCTime
  , extraVolumes     :: [Volume]
  , fromImage        :: Maybe Text
  , fromServer       :: Maybe Text
  , marketplaceKey   :: Maybe Text
  , modificationDate :: UTCTime
  , organization     :: OrganizationId
  , public           :: Bool
  , rootVolume       :: VolumRef
} deriving (Show, Eq, Generic)

instance FromJSON ImageId
instance ToJSON ImageId

instance FromJSON VolumRef
instance ToJSON VolumRef

instance FromJSON Image
instance ToJSON Image
