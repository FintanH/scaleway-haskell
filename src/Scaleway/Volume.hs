{-# LANGUAGE DeriveGeneric #-}

module Scaleway.Volume
    ( Volume(..)
    , VolumeId
    ) where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

newtype VolumeId = VolumeId Text deriving (Show, Eq, Generic)

data Volume = Volume deriving (Show, Eq, Generic)

instance FromJSON VolumeId
instance ToJSON VolumeId

instance FromJSON Volume
instance ToJSON Volume
