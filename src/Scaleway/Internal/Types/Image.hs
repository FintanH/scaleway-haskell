{-# LANGUAGE DuplicateRecordFields #-}

module Scaleway.Internal.Types.Image where

import Scaleway.Internal.Types.ResourceId (parseImageId, ImageId)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?), Value(Object))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time (UTCTime)

type ImageName = Text

data ImageBase org volume = ImageBase {
    architecture :: Text
  , name         :: ImageName
  , organization :: org
  , rootVolume   :: volume
} deriving (Show, Eq)

data ImageRef = ImageRef {
    imageId   :: ImageId
  , name :: ImageName
} deriving (Show, Eq)

instance FromJSON ImageRef where
  parseJSON = withObject "image ref" $ \o -> do
    imageId <- parseImageId (Object o)
    name <- o .: "name"
    return ImageRef {..}

parseImageBase :: (Value -> Parser org)
               -> (Value -> Parser volume)
               -> (Value -> Parser (ImageBase org volume))
parseImageBase orgParser volumeParser = withObject "image base" $ \o -> do
  architecture <- o .: "arch"
  name <- o .: "name"
  organization <- orgParser (Object o)
  rootVolume <- volumeParser (Object o)
  return ImageBase {..}
