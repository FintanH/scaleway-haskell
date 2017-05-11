{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Scaleway.Internal.Types.Volume where

import           Data.Aeson   (FromJSON, parseJSON, withObject, (.:), Value (..))
import Data.Aeson.Types (Parser)
import           Data.Text    (Text)
import           GHC.Generics
import Scaleway.Internal.Types.ResourceId (VolumeId, parseVolumeId)
import Control.Lens (makeLenses)

type VolumeName = Text

data VolumeBase org = VolumeBase {
    volumeBaseName         :: VolumeName
  , volumeBaseOrganization :: org
  , volumeBaseSize         :: Int
  , volumeBaseVolumeType   :: Text
} deriving (Show, Eq)

makeLenses ''VolumeBase

data VolumeRef = VolumeRef {
    volumeRefName :: VolumeName
  , volumeId :: VolumeId
} deriving (Show, Eq)

instance FromJSON VolumeRef where
  parseJSON = withObject "server ref" $ \o -> do
    volumeId <- parseVolumeId (Object o)
    volumeRefName <- o .: "name"
    return VolumeRef {..}

parseVolumeBase :: (Value -> Parser org) -> (Value -> Parser (VolumeBase org))
parseVolumeBase orgParser = withObject "volume base" $ \o -> do
  volumeBaseName <- o .: "name"
  volumeBaseSize <- o .: "size"
  volumeBaseVolumeType <- o .: "volume_type"
  volumeBaseOrganization <- orgParser (Object o)
  return VolumeBase {..}
