{-# LANGUAGE DuplicateRecordFields #-}

module Scaleway.Internal.Types.Server where

import           Data.Aeson                (FromJSON, ToJSON, genericParseJSON,
                                            parseJSON, Value (..), (.:), (.:?), withObject, withText)
import Data.Aeson.Types (Parser)
import           Data.Aeson.Types          (Options (..), defaultOptions)
import           Data.Text                 (Text, unpack)
import           GHC.Generics
import           Scaleway.Internal.Utility (jsonCamelCase)
import Data.Monoid ((<>))
import Scaleway.Internal.Types.ResourceId (ServerId, parseServerId)

type ServerName = Text

data ServerBase org image tags = ServerBase {
      name           :: ServerName
    , organization   :: org
    , image          :: image
    , commercialType :: CommercialType
    , tags           :: tags
    , enableIpv6     :: Maybe Bool
} deriving (Show, Eq, Generic)

data ServerRef = ServerRef {
    serverId :: ServerId
  , name     :: ServerName
} deriving (Show, Eq)

instance FromJSON ServerRef where
  parseJSON = withObject "server ref" $ \o -> do
    serverId <- parseServerId (Object o)
    name <- o .: "name"
    return ServerRef {..}

data BootScript = BootScript {
    kernel         :: Text
  , initrd         :: Text
  , bootcmdargs    :: Text
  , architecture   :: Text
  , title          :: Text
  , dtb            :: Text
  , organizationId :: Text
  , bootScriptId   :: Text
  , public         :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON BootScript where
  parseJSON = genericParseJSON opts . jsonCamelCase
    where
      opts = defaultOptions { fieldLabelModifier = modifyNames }
      modifyNames "bootScriptId" = "id"
      modifyNames "organizationId" = "id"
      modifyNames x            = x

instance ToJSON BootScript

data PublicIp = PublicIp {
    dynamic    :: Bool
  , publicIpId :: Text
  , address    :: Text
} deriving (Show, Eq, Generic)

instance FromJSON PublicIp where
  parseJSON = genericParseJSON defaultOptions . jsonCamelCase

instance ToJSON PublicIp

data ServerState = Running
                 | Stopped
                 | Booted
                 deriving (Eq, Generic)

instance Show ServerState where
  show Running = "running"
  show Stopped = "stopped"
  show Booted  = "booted"

instance FromJSON ServerState where
  parseJSON = withText "server state" $ \t ->
    case t of
      "running" -> return Running
      "stopped" -> return Stopped
      "booted" -> return Booted
      _        -> fail (unpack $ "Server state " <> t <> " was not recognised")
instance ToJSON ServerState

data CommercialType = VC1S
                    | VC1M
                    | VC1L
                    | C1
                    | C2S
                    | C2M
                    | C2L
                    deriving (Show, Eq, Generic)

instance FromJSON CommercialType
instance ToJSON CommercialType

-- | Instances
instance (FromJSON org, FromJSON image, FromJSON tags)
      => FromJSON (ServerBase org image tags) where
  parseJSON = genericParseJSON opts . jsonCamelCase
    where
      opts = defaultOptions { fieldLabelModifier = modifyNames }
      modifyNames x            = x

parseServerBase :: (Value -> Parser org)
                -> (Value -> Parser image)
                -> (Value -> Parser tags)
                -> (Value -> Parser (ServerBase org image tags))
parseServerBase orgParser imageParser tagsParser = withObject "server base" $ \o -> do
  let object = (Object o)
  name <- o .: "name"
  organization <- orgParser object
  image <- imageParser object
  commercialType <- o .: "commercial_type"
  tags <- tagsParser object
  enableIpv6 <- o .:? "enable_ipv6"
  return ServerBase {..}

instance (ToJSON org, ToJSON image, ToJSON tags)
      => ToJSON (ServerBase org image tags)
