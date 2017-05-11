{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Scaleway.Internal.Types.Image where

import Scaleway.Internal.Types.ResourceId (parseImageId, ImageId)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?), Value(Object))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time (UTCTime)
import Control.Lens (makeLenses)

type ImageName = Text

data ImageBase org volume = ImageBase {
    imageBaseArchitecture :: Text
  , imageBaseName         :: ImageName
  , imageBaseOrganization :: org
  , imageBaseRootVolume   :: volume
} deriving (Show, Eq)

makeLenses ''ImageBase

data ImageRef = ImageRef {
    imageRefId   :: ImageId
  , imageRefName :: ImageName
} deriving (Show, Eq)

instance FromJSON ImageRef where
  parseJSON = withObject "image ref" $ \o -> do
    imageRefId <- parseImageId (Object o)
    imageRefName <- o .: "name"
    return ImageRef {..}

parseImageBase :: (Value -> Parser org)
               -> (Value -> Parser volume)
               -> (Value -> Parser (ImageBase org volume))
parseImageBase orgParser volumeParser = withObject "image base" $ \o -> do
  imageBaseArchitecture <- o .: "arch"
  imageBaseName <- o .: "name"
  imageBaseOrganization <- orgParser (Object o)
  imageBaseRootVolume <- volumeParser (Object o)
  return ImageBase {..}
