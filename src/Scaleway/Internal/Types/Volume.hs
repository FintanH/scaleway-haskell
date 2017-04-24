{-# LANGUAGE DuplicateRecordFields #-}

module Scaleway.Internal.Types.Volume where

import           Data.Aeson   (FromJSON, parseJSON, withObject, (.:), Value (..))
import Data.Aeson.Types (Parser)
import           Data.Text    (Text)
import           GHC.Generics
import Scaleway.Internal.Types.ResourceId (VolumeId, parseVolumeId)

type VolumeName = Text

data VolumeBase org = VolumeBase {
    name         :: VolumeName
  , organization :: org
  , size         :: Int
  , volumeType   :: Text
} deriving (Show, Eq)

data VolumeRef = VolumeRef {
    name :: VolumeName
  , volumeId :: VolumeId
} deriving (Show, Eq)

instance FromJSON VolumeRef where
  parseJSON = withObject "server ref" $ \o -> do
    volumeId <- parseVolumeId (Object o)
    name <- o .: "name"
    return VolumeRef {..}

parseVolumeBase :: (Value -> Parser org) -> (Value -> Parser (VolumeBase org))
parseVolumeBase orgParser = withObject "volume base" $ \o -> do
  name <- o .: "name"
  size <- o .: "size"
  volumeType <- o .: "volume_type"
  organization <- orgParser (Object o)
  return VolumeBase {..}
