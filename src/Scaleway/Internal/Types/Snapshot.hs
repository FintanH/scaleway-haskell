{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Scaleway.Internal.Types.Snapshot where

import Data.Text (Text, unpack)
import Data.Aeson (FromJSON, parseJSON, Value(Object), withObject, (.:))
import Data.Monoid ((<>))
import Data.Aeson.Types (Parser, withText)
import Control.Lens (makeLenses)

type SnapshotName = Text

data SnapshotBase org volume = SnapshotBase {
    snapshotBaseName         :: SnapshotName
  , snapshotBaseOrganization :: org
  , snapshotBaseVolume       :: volume
} deriving (Show, Eq)

makeLenses ''SnapshotBase

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
  snapshotBaseName <- o .: "name"
  snapshotBaseOrganization <- orgParser (Object o)
  snapshotBaseVolume <- volumeParser (Object o)
  return SnapshotBase {..}
