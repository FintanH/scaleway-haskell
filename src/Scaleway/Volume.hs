{-# LANGUAGE DeriveGeneric #-}

module Scaleway.Volume
    ( Volume
    , VolumeId
    ) where

import           Data.Aeson
import           Data.Text             (Text)
import           GHC.Generics
import           Scaleway.Organization (OrganizationId)
import           Scaleway.Server       (Server)

newtype VolumeId = VolumeId Text deriving (Show, Eq, Generic)

data Volume = Volume {
    volumeId     :: VolumeId
  , volumeName   :: Text
  , exportURI    :: Maybe Bool
  , organization :: OrganizationId
  , server       :: Maybe Server
  , size         :: Int
  , volumeType   :: Text
} deriving (Show, Eq, Generic)

instance FromJSON VolumeId
instance ToJSON VolumeId

instance FromJSON Volume
instance ToJSON Volume
