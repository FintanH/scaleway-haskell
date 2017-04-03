{-# LANGUAGE DeriveGeneric #-}

module Scaleway.Server
    ( Server
    ) where

import           Data.Aeson
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics
import           Scaleway.Image        (Image, ImageId)
import           Scaleway.Organization (OrganizationId)
import           Scaleway.Volume       (Volume)

newtype ServerId = ServerId Text deriving (Show, Eq, Generic)

data ServerState = Running
                 | Stopped
                 | Booted
                 deriving (Eq, Generic)

data ImageRef = ImageRef {
    imageID   :: ImageId
  , imageName :: Text
} deriving (Show, Eq, Generic)

data Server = Server {
    serverID        :: ServerId
  , severName       :: Text
  , image           :: ImageRef
  , bootscript      :: Maybe Text
  , dynamicPublicIP :: Bool
  , organization    :: OrganizationId
  , privateIP       :: Maybe Text
  , publicIP        :: Maybe Text
  , state           :: ServerState
  , tags            :: [Text]
  , volumes         :: [Volume]
} deriving (Show, Eq, Generic)

instance FromJSON ServerId
instance ToJSON ServerId

instance FromJSON ImageRef
instance ToJSON ImageRef

instance FromJSON Server
instance ToJSON Server

instance Show ServerState where
  show Running = "running"
  show Stopped = "stopped"
  show Booted = "booted"

instance FromJSON ServerState
instance ToJSON ServerState

getImage :: Server -> Image
getImage = undefined
