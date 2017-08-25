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


-- data AccountResource = AccountOrganizations Organization
--                      | AccountUsers User
--                      | AccountToken Token

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


newtype ImageResult = ImageResult { image :: Image }
  deriving (Show, Eq, Generic)

instance FromJSON ImageResult


imageCreatePrefix :: String
imageCreatePrefix = "imageCreate"

data ImageCreate = ImageCreate {
    imageCreateOrganization :: Text
  , imageCreateArch         :: Text
  , imageCreateName         :: Text
  , imageCreateRootVolume   :: Text
} deriving (Show, Eq, Generic)

instance ToJSON ImageCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length imageCreatePrefix) }


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

instance ToJSON Role where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modify }
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


newtype VolumeResult = VolumeResult { volume :: Volume }
  deriving (Show, Eq, Generic)

instance FromJSON VolumeResult


volumeCreatePrefix :: String
volumeCreatePrefix = "volumeCreate"

data VolumeCreate = VolumeCreate {
    volumeCreateName         :: Text
  , volumeCreateOrganization :: Text
  , volumeCreate             :: Int
  , volumeCreateVolumeType   :: Text
} deriving (Show, Eq, Generic)

instance ToJSON VolumeCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length volumeCreatePrefix) }


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

securityGroupPrefix :: String
securityGroupPrefix = "securityGroupRef"

data SecurityGroup = SecurityGroup {
    securityGroupId                    :: Text
  , securityGroupName                  :: Text
  , securityGroupOrganization          :: Text
  , securityGroupDescription           :: Text
  , securityGroupEnableDefaultSecurity :: Bool
  , securityGroupOrganizationDefault   :: Bool
  , securityGroupServers               :: [ServerRef]
} deriving (Show, Eq, Generic)

instance FromJSON SecurityGroup where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length securityGroupPrefix) }

instance ToJSON SecurityGroup where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length securityGroupPrefix) }


newtype SecurityGroups = SecurityGroups { securityGroups :: [SecurityGroup] }
  deriving (Show, Eq, Generic)

instance FromJSON SecurityGroups where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }


newtype SecurityGroupResult = SecurityGroupResult { securityGroup :: SecurityGroup }
  deriving (Show, Eq, Generic)

instance FromJSON SecurityGroupResult where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }


securityGroupCreate :: String
securityGroupCreate = "securityGroupCreate"

data SecurityGroupCreate = SecurityGroupCreate {
    securityGroupCreateOrganization :: Text
  , securityGroupCreateName         :: Text
  , securityGroupCreateDescription  :: Text
} deriving (Show, Eq, Generic)

instance ToJSON SecurityGroupCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length securityGroupCreate) }


-------------------------------------------------------------------------------


snapshotPrefix :: String
snapshotPrefix = "snapshot"

data Snapshot = Snapshot {
    snapshotId           :: Text
  , snapshotName         :: Text
  , snapshotOrganization :: Text
  , snapshotBaseVolume   :: VolumeRef
  , snapshotCreationDate :: UTCTime
  , snapshotSize         :: Int
  , snapshotState        :: SnapshotState
  , snapshotVolumeType   :: Text
} deriving (Show, Eq, Generic)

instance FromJSON Snapshot where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length snapshotPrefix) }

instance ToJSON Snapshot where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length snapshotPrefix) }


data SnapshotState =
    Snapshotting
  | Snapshotted
  deriving (Show, Eq)

instance FromJSON SnapshotState where
  parseJSON = withText "snap shot state" $ \t ->
    case t of
      "snapshotting" -> pure Snapshotting
      "snapshotted"  -> pure Snapshotted
      _              -> fail ("Could not make SnapshotState type with: " ++ unpack t)

instance ToJSON SnapshotState where
  toJSON = String . pack . map toLower . show


newtype Snapshots = Snapshots { snapshots :: [Snapshot] }
  deriving (Show, Eq, Generic)

instance FromJSON Snapshots


newtype SnapshotResult = SnapshotResult { snapshot :: Snapshot }
  deriving (Show, Eq, Generic)

instance FromJSON SnapshotResult


snapshotCreate :: String
snapshotCreate = "snapshotCreate"

data SnapshotCreate = SnapshotCreate {
    snapshotCreateName         :: Text
  , snapshotCreateOrganization :: Text
  , snapshotCreateVolumeId     :: Text
} deriving (Show, Eq, Generic)

instance ToJSON SnapshotCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length snapshotCreate) }


-------------------------------------------------------------------------------

tokenPrefix :: String
tokenPrefix = "token"

data Token = Token {
    tokenId                :: Text
  , tokenCreationDate      :: UTCTime
  , tokenExpires           :: Bool
  , tokenInheritsUserPerms :: Bool
  , tokenPermissions       :: [Text]
  , tokenRoles             :: Role
} deriving (Show, Eq, Generic)

instance FromJSON Token where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length tokenPrefix) }

instance ToJSON Token where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length tokenPrefix) }


newtype Tokens = Tokens { tokens :: [Token] }
  deriving (Show, Eq, Generic)

instance FromJSON Tokens


newtype TokenResult = TokenResult { token :: Token }
  deriving (Show, Eq, Generic)

instance FromJSON TokenResult


tokenCreatePrefix :: String
tokenCreatePrefix = "tokenCreate"

data TokenCreate = TokenCreate {
    tokenCreateEmail    :: Text
  , tokenCreatePassword :: Text
  , tokenCreateExpires  :: Bool
} deriving (Show, Eq, Generic)

instance ToJSON TokenCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length tokenCreatePrefix) }


-------------------------------------------------------------------------------

securityRulePrefix :: String
securityRulePrefix = "securityRule"

data SecurityRule = SecurityRule {
    securityRuleId           :: Text
  , securityRuleAction       :: Text
  , securityRuleDirection    :: Direction
  , securityRuleProtocol     :: Protocol
  , securityRuleIpRange      :: Text
  , securityRuleDestPortFrom :: Maybe Int
  , securityRuleDestPortTo   :: Maybe Int
  , securityRulePosition     :: Int
  , securityRuleEditable     :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON SecurityRule where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length securityRulePrefix) }

instance ToJSON SecurityRule where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length securityRulePrefix) }

data SecurityRules = SecurityRules { securityRules :: [SecurityRule] }
  deriving (Show, Eq, Generic)

instance FromJSON SecurityRules where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }


data SecurityRuleResult = SecurityRuleResult { securityRule :: SecurityRule }
  deriving (Show, Eq, Generic)

instance FromJSON SecurityRuleResult where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }


securityRuleCreatePrefix :: String
securityRuleCreatePrefix = "securityRuleCreate"

data SecurityRuleCreate = SecurityRuleCreate {
    securityRuleCreateAction    :: Text
  , securityRuleCreateDirection :: Direction
  , securityRuleCreateIpRange   :: Text
  , securityRuleCreateProtocol  :: Protocol
} deriving (Show, Eq, Generic)

instance ToJSON SecurityRuleCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length securityRuleCreatePrefix) }


data Direction = Inbound
               | Outbound
               deriving (Show, Eq)

instance FromJSON Direction where
  parseJSON = withText "direction" $ \t ->
    case t of
      "inbound"  -> return Inbound
      "outbound" -> return Outbound
      _          -> fail $ "Could not parse direction: " ++ unpack t

instance ToJSON Direction where
  toJSON = String . pack . map toLower . show

data Protocol = TCP
              | UDP
              deriving (Show, Eq, Generic)

instance FromJSON Protocol

instance ToJSON Protocol


-------------------------------------------------------------------------------


data Region =
    Paris
  | Amsterdam
  deriving (Eq)

instance Show Region where
  show Paris     = "par1"
  show Amsterdam = "ams1"
