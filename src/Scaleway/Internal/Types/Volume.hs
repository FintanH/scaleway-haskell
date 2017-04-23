
module Scaleway.Internal.Types.Volume
    ( VolumeName
    , VolumeBase (..)
    , parseVolumeBase
    ) where

import           Data.Aeson   (FromJSON, withObject, (.:), Value (..))
import Data.Aeson.Types (Parser)
import           Data.Text    (Text)
import           GHC.Generics

type VolumeName = Text

data VolumeBase org = VolumeBase {
    name         :: VolumeName
  , organization :: org
  , size         :: Int
  , volumeType   :: Text
} deriving (Show, Eq)

parseVolumeBase :: (Value -> Parser org) -> (Value -> Parser (VolumeBase org))
parseVolumeBase orgParser = withObject "volume base" $ \o -> do
  name <- o .: "name"
  size <- o .: "size"
  volumeType <- o .: "volume_type"
  organization <- orgParser (Object o)
  return VolumeBase {..}
