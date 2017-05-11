{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Control.Lens (makeLenses)

type ServerName = Text

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

data ServerBase org image tags = ServerBase {
      serverBaseName :: ServerName
    , serverBaseOrganization   :: org
    , serverBaseImage          :: image
    , serverBaseCommercialType :: CommercialType
    , serverBaseTags           :: tags
    , serverBaseEnableIpv6     :: Maybe Bool
} deriving (Show, Eq, Generic)

makeLenses ''ServerBase

data ServerRef = ServerRef {
    serverRefServerId :: ServerId
  , serverRefName     :: ServerName
} deriving (Show, Eq)

instance FromJSON ServerRef where
  parseJSON = withObject "server ref" $ \o -> do
    serverRefServerId <- parseServerId (Object o)
    serverRefName <- o .: "name"
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
  parseJSON = withObject "public ip" $ \o -> do
    dynamic <- o .: "dynamic"
    publicIpId <- o .: "id"
    address <- o .: "address"
    pure PublicIp {..}

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

parseServerBase :: (Value -> Parser org)
                -> (Value -> Parser image)
                -> (Value -> Parser tags)
                -> (Value -> Parser (ServerBase org image tags))
parseServerBase orgParser imageParser tagsParser = withObject "server base" $ \o -> do
  let object = (Object o)
  serverBaseName <- o .: "name"
  serverBaseOrganization <- orgParser object
  serverBaseImage <- imageParser object
  serverBaseCommercialType <- o .: "commercial_type"
  serverBaseTags <- tagsParser object
  serverBaseEnableIpv6 <- o .:? "enable_ipv6"
  return ServerBase {..}
