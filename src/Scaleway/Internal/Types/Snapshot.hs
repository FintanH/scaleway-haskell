
module Scaleway.Internal.Types.Snapshot where

import Data.Text (Text, unpack)
import Data.Aeson (FromJSON, parseJSON, Value(Object), withObject, (.:))
import Data.Monoid ((<>))
import Data.Aeson.Types (Parser, withText)

type SnapshotName = Text

data SnapshotBase org volume = SnapshotBase {
    name         :: SnapshotName
  , organization :: org
  , volume       :: volume
} deriving (Show, Eq)

data SnapshotState =
    Snapshotting
  | Snapshotted
  deriving (Show, Eq)

instance FromJSON SnapshotState where
  parseJSON = withText "snap shot state" $ \t ->
    case t of
      "snapshotting" -> pure Snapshotting
      "snapshotted"  -> pure Snapshotted
      _              -> fail (unpack $ "Could not make SnapshotState type with: " <> t)

parseSnapshotBase :: (Value -> Parser org)
                  -> (Value -> Parser volume)
                  -> (Value -> Parser (SnapshotBase org volume))
parseSnapshotBase orgParser volumeParser = withObject "snapshot base" $ \o -> do
  name <- o .: "name"
  organization <- orgParser (Object o)
  volume <- volumeParser (Object o)
  return SnapshotBase {..}
