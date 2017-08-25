{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Scaleway.Types where

import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      Value (Array, String), genericParseJSON,
                                      genericToJSON, withObject, withText, (.:))
import           Data.Aeson.Casing   (snakeCase)
import           Data.Aeson.TH       (defaultOptions, fieldLabelModifier)
import           Data.Char           (toLower)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text, pack, unpack)
import           Data.Time           (UTCTime)
import           GHC.Generics


-------------------------------------------------------------------------------


serverPrefix :: String
serverPrefix = "server"

data Server = Server {
    serverId             :: Text
  , serverName           :: Text
  , serverOrganization   :: Text
  , serverImage          :: Image
  , serverCommercialType :: CommercialType
  , serverTags           :: [Text]
  , serverEnableIpv6     :: Maybe Bool
  , serverBootscript     :: Maybe BootScript
  , serverPrivateIp      :: Maybe Text
  , serverPublicIp       :: Maybe PublicIpRef
  , serverState          :: ServerState
  , serverVolumes        :: HM.HashMap Int Volume
} deriving (Show, Eq, Generic)

instance FromJSON Server where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverPrefix) }

instance ToJSON Server where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverPrefix) }

newtype Servers = Servers { servers :: [Server] } deriving (Show, Eq, Generic)

instance FromJSON Servers


newtype ServerResult = ServerResult { server :: Server }
  deriving (Show, Eq, Generic)

instance FromJSON ServerResult


serverCreatePrefix :: String
serverCreatePrefix = "serverCreate"

data ServerCreate = ServerCreate {
    serverCreateOrganization   :: Text
  , serverCreateName           :: Text
  , serverCreateCommercialType :: CommercialType
  , serverCreateImage          :: Text
  , serverCreateTags           :: [Text]
  , serverCreateEnableIpv6     :: Bool
} deriving (Show, Eq, Generic)

instance ToJSON ServerCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverCreatePrefix) }


serverRefPrefix :: String
serverRefPrefix = "serverRef"

data ServerRef = ServerRef {
    serverRefId   :: Text
  , serverRefName :: Text
} deriving (Show, Eq, Generic)

instance FromJSON ServerRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverRefPrefix) }

instance ToJSON ServerRef where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverRefPrefix) }


-------------------------------------------------------------------------------


bootScriptPrefix :: String
bootScriptPrefix = "bootScript"

data BootScript = BootScript {
    bootScriptKernel       :: Text
  , bootScriptInitrd       :: Text
  , bootScriptBootcmdargs  :: Text
  , bootScriptArchitecture :: Text
  , bootScriptTitle        :: Text
  , bootScriptDtb          :: Text
  , bootScriptOrganization :: Text
  , bootScriptId           :: Text
  , bootScriptPublic       :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON BootScript where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length bootScriptPrefix) }


instance ToJSON BootScript where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length bootScriptPrefix) }


-------------------------------------------------------------------------------


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
      "running" -> pure Running
      "stopped" -> pure Stopped
      "booted"  -> pure Booted
      _         -> fail ("Server state " ++ (unpack t) ++ " was not recognised")

instance ToJSON ServerState where
  toJSON = String . pack . map toLower . show


-------------------------------------------------------------------------------

data CommercialType = VC1S
                    | VC1M
                    | VC1L
                    | C1
                    | C2S
                    | C2M
                    | C2L
                    | ARM64_128GB
                    deriving (Show, Eq)

instance FromJSON CommercialType where
  parseJSON = withText "commercial_type" $ \t ->
    case t of
      "VC1S"        -> pure VC1S
      "VC1M"        -> pure VC1M
      "VC1L"        -> pure VC1L
      "C1"          -> pure C1
      "C2S"         -> pure C2S
      "C2M"         -> pure C2M
      "C2L"         -> pure C2L
      "ARM64-128GB" -> pure ARM64_128GB
      _             -> fail $ "Unknown commercial_type: " ++ (unpack t)

instance ToJSON CommercialType where
  toJSON ct = case ct of
    ARM64_128GB -> String "ARM64-128GB"
    ct          -> String . pack . show $ ct

-------------------------------------------------------------------------------


data Action = PowerOn
            | PowerOff
            | Reboot
            | Terminate
            deriving (Eq, Show)

instance FromJSON Action where
  parseJSON = withText "action" $ \t ->
    case t of
      "poweroff"  -> pure PowerOff
      "poweron"   -> pure PowerOn
      "reboot"    -> pure Reboot
      "terminate" -> pure Terminate
      _           -> fail $ "Unknown action: " ++ (unpack t)

instance ToJSON Action where
  toJSON = String . pack . map toLower . show


newtype Actions = Actions { actions :: [Action] }
  deriving (Show, Eq, Generic)

instance FromJSON Actions


newtype ActionRequest = ActionRequest { action :: Action }
  deriving (Show, Eq, Generic)

instance ToJSON ActionRequest

taskPrefix :: String
taskPrefix = "task"

data Task = Task {
    taskDescription :: Text
  , taskHrefFrom    :: Text
  , taskId          :: Text
  , taskProgress    :: Text
  , taskStatus      :: Text
} deriving (Show, Eq, Generic)

instance FromJSON Task where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length taskPrefix) }


data ActionResponse = ActionResponse { task :: Task }
  deriving (Show, Eq, Generic)

instance FromJSON ActionResponse


-------------------------------------------------------------------------------


organizationPrefix :: String
organizationPrefix = "organization"

data Organization = Organization {
    organizationId    :: Text
  , organizationName  :: Text
  , organizationUsers :: [User]
} deriving (Show, Eq, Generic)

instance FromJSON Organization where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length organizationPrefix) }


newtype Organizations = Organizations { organizations :: [Organization] }
  deriving (Show, Eq, Generic)

instance FromJSON Organizations


-------------------------------------------------------------------------------


imagePrefix :: String
imagePrefix = "image"

data Image = Image {
    imageId               :: Text
  , imageName             :: Text
  , imageOrganization     :: Text
  , imageRootVolume       :: VolumeRef
  , imageArch             :: Text
  , imageCreationDate     :: UTCTime
  , imageExtraVolumes     :: Text
  , imageFromImage        :: Maybe Text
  , imageFromServer       :: Maybe Text
  , imageMarketplaceKey   :: Maybe Text
  , imageModificationDate :: UTCTime
  , imagePublic           :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Image where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length imagePrefix) }

instance ToJSON Image where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length imagePrefix) }

newtype Images = Images { images :: [Image] } deriving (Show, Eq, Generic)

instance FromJSON Images


-------------------------------------------------------------------------------


userPrefix :: String
userPrefix = "user"

data User = User {
    userId            :: Text
  , userEmail         :: Text
  , userFirstname     :: Text
  , userLastname      :: Text
  , userFullname      :: Text
  , userOrganizations :: Maybe [Text]
  , userRoles         :: Maybe [Role]
  , userSshPublicKeys :: Maybe [Text]
} deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length userPrefix) }


-------------------------------------------------------------------------------


data RoleType = Manager
  deriving (Show, Eq, Generic)

instance FromJSON RoleType
instance ToJSON RoleType

rolePrefix :: String
rolePrefix = "role"

data Role = Role {
    roleOrganization :: Maybe Text
  , roleType         :: Maybe RoleType
} deriving (Show, Eq, Generic)

instance FromJSON Role where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }
    where
      modify "roleType" = "role"
      modify s          = snakeCase . drop (length rolePrefix) $ s


-------------------------------------------------------------------------------


volumePrefix :: String
volumePrefix = "volume"

data Volume = Volume {
      volumeId               :: Text
    , volumeName             :: Text
    , volumeOrganization     :: Text
    , volumeSize             :: Int
    , volumeType             :: Text
    , volumeModificationDate :: Maybe UTCTime
    , volumeCreationDate     :: Maybe UTCTime
    , volumeExportURI        :: Maybe Text
    , volumeServer           :: Maybe ServerRef
} deriving (Show, Eq, Generic)

instance FromJSON Volume where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modify }
    where
      modify "volumeType" = "volume_type"
      modify s            = snakeCase . drop (length volumePrefix) $ s

instance ToJSON Volume where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }
    where
      modify "volumeType" = "volume_type"
      modify s            = snakeCase . drop (length volumePrefix) $ s


newtype Volumes = Volumes { volumes :: [Volume] } deriving (Show, Eq, Generic)

instance FromJSON Volumes


volumeRefPrefix :: String
volumeRefPrefix = "volumeRef"

data VolumeRef = VolumeRef {
    volumeRefName :: Text
  , volumeRefId   :: Text
} deriving (Show, Eq, Generic)

instance FromJSON VolumeRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length volumeRefPrefix) }

instance ToJSON VolumeRef where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length volumeRefPrefix) }


-------------------------------------------------------------------------------

publicIpPrefix :: String
publicIpPrefix = "publicIp"

data PublicIp = PublicIp {
    publicIpId           :: Text
  , publicIpAddress      :: Text
  , publicIpOrganization :: Text
  , publicIpServer       :: ServerRef
} deriving (Show, Eq, Generic)

instance FromJSON PublicIp where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length publicIpPrefix) }


newtype PublicIps = PublicIps { ips :: [PublicIp] } deriving (Show, Eq, Generic)

instance FromJSON PublicIps


publicIpRefPrefix :: String
publicIpRefPrefix = "publicIpRef"

data PublicIpRef = PublicIpRef {
    publicIpRefId      :: Text
  , publicIpRefAddress :: Text
  , publicIpRefDynamic :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON PublicIpRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length publicIpRefPrefix) }


instance ToJSON PublicIpRef where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length publicIpRefPrefix) }


-------------------------------------------------------------------------------


data Region =
    Paris
  | Amsterdam
  deriving (Eq)

instance Show Region where
  show Paris     = "par1"
  show Amsterdam = "ams1"
