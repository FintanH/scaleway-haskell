{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Scaleway.Types.Internal
    ( ServerId(..)
    , ImageId(..)
    , OrganizationId(..)
    , UserId(..)
    , VolumeId(..)
    , SnapshotId(..)
    , IpId(..)
    , SecurityGroupId(..)
    , SecurityRuleId(..)
    , TokenId(..)
    , ServerState(..)
    , CommercialType(..)
    , Role(..)
    , SnapshotState(..)
    , Direction(..)
    , Protocol(..)
    , BootScript(..)
    , PublicIp(..)
    , Tag(..)
    , Region(..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON, genericParseJSON,
                                            parseJSON, withText)
import           Data.Aeson.Types          (Options (..), defaultOptions)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import           Scaleway.Internal.Utility (jsonCamelCase)

-- | ID aliases

newtype ServerId = ServerId Text deriving (Show, Eq, Generic)

newtype ImageId = ImageId Text deriving (Show, Eq, Generic)

newtype OrganizationId = OrganizationId Text deriving (Show, Eq, Generic)

newtype UserId = UserId Text deriving (Show, Eq, Generic)

newtype VolumeId = VolumeId Text deriving (Show, Eq, Generic)

newtype SnapshotId = SnapshotId Text deriving (Show, Eq, Generic)

newtype IpId = IpId Text deriving (Show, Eq, Generic)

newtype SecurityGroupId = SecurityGroupId Text deriving (Show, Eq, Generic)

newtype SecurityRuleId = SecurityRuleId Text deriving (Show, Eq, Generic)

data TokenId = TokenId Text deriving (Show, Eq, Generic)

-- | Other data types that aren't directly involved in GET/POST/DELETE requests
data ServerState = Running
                 | Stopped
                 | Booted
                 deriving (Eq, Generic)

data CommercialType = VC1S
                    | VC1M
                    | VC1L
                    | C1
                    | C2S
                    | C2M
                    | C2L
                    | ARM64
                    deriving (Show, Eq, Generic)

data Role = Manager deriving (Show, Eq, Generic)

data SnapshotState = Snapshotting
                   | Snapshotted
                   deriving (Show, Eq, Generic)

data Direction = Inbound
               | Outbound
               deriving (Show, Eq, Generic)

data Protocol = TCP
              | UDP
              deriving (Show, Eq, Generic)


data BootScript = BootScript {
    kernel       :: Text
  , initrd       :: Text
  , bootcmdargs  :: Text
  , architecture :: Text
  , title        :: Text
  , dtb          :: Text
  , organization :: Text
  , id           :: Text
  , public       :: Bool
} deriving (Show, Eq, Generic)


data PublicIp = PublicIp {
    dynamic    :: Bool
  , publicIpId :: Text
  , address    :: Text
} deriving (Show, Eq, Generic)

newtype Tag = Tag Text deriving (Show, Eq, Generic)

data Region = Paris
            | Amsterdam
            deriving (Eq)

instance Show Region where
  show Paris     = "par1"
  show Amsterdam = "ams1"

instance FromJSON ServerId
instance ToJSON ServerId

instance FromJSON ImageId
instance ToJSON ImageId

instance FromJSON OrganizationId
instance ToJSON OrganizationId

instance FromJSON UserId
instance ToJSON UserId

instance FromJSON VolumeId
instance ToJSON VolumeId

instance FromJSON SnapshotId
instance ToJSON SnapshotId

instance FromJSON IpId
instance ToJSON IpId

instance FromJSON SecurityGroupId
instance ToJSON SecurityGroupId

instance FromJSON SecurityRuleId
instance ToJSON SecurityRuleId

instance FromJSON TokenId
instance ToJSON TokenId

instance Show ServerState where
  show Running = "running"
  show Stopped = "stopped"
  show Booted  = "booted"

instance FromJSON ServerState where
  parseJSON = withText "server state" $ \t ->
    case toState t of
      Right s  -> pure s
      Left err -> fail err
    where
      toState "running"    = Right Running
      toState "stopped"    = Right Stopped
      toState "booted"     = Right Booted
      toState unknownState = Left $ T.unpack ("Unknown state: " <> unknownState)

instance ToJSON ServerState

instance FromJSON CommercialType where
  parseJSON = withText "commercial type" $ \t ->
    case toCommercialType t of
      Right s  -> pure s
      Left err -> fail err
    where
      toCommercialType "VC1S"        = Right VC1S
      toCommercialType "VC1M"        = Right VC1M
      toCommercialType "VC1L"        = Right VC1L
      toCommercialType "C1"          = Right C1
      toCommercialType "C2S"         = Right C2S
      toCommercialType "C2M"         = Right C2M
      toCommercialType "C2L"         = Right C2L
      toCommercialType "ARM64-128GB" = Right ARM64
      toCommercialType unknownType   = Left $ T.unpack ("Unknown commercial type: " <> unknownType)

instance ToJSON CommercialType

instance FromJSON Role
instance ToJSON Role

instance FromJSON SnapshotState where
  parseJSON = withText "snap shot state" $ \t ->
    case t of
      "snapshotting" -> return Snapshotting
      "snapshotted"  -> return Snapshotted
      _              -> fail (T.unpack $ "Could not make SnapshotState type with: " <> t)

instance ToJSON SnapshotState

instance FromJSON Direction
instance ToJSON Direction

instance FromJSON Protocol
instance ToJSON Protocol

instance FromJSON BootScript
instance ToJSON BootScript

instance FromJSON PublicIp where
  parseJSON = genericParseJSON opts . jsonCamelCase
    where
      opts = defaultOptions { fieldLabelModifier = modifyNames }
      modifyNames "publicIpId" = "id"
      modifyNames x            = x

instance ToJSON PublicIp

instance FromJSON Tag
instance ToJSON Tag
