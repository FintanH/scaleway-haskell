{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Scaleway.Types where

import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      Value (String), genericParseJSON,
                                      genericToJSON, withText)
import           Data.Aeson.Casing   (snakeCase)
import           Data.Aeson.TH       (defaultOptions, fieldLabelModifier)
import           Data.Char           (toLower)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text, pack, unpack)
import           Data.Time           (UTCTime)
import           GHC.Generics
import           Servant.API         (ToHttpApiData)


-------------------------------------------------------------------------------


newtype ServerId = ServerId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

newtype ServerName = ServerName Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

newtype Tags = Tags [Text]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


serverPrefix :: String
serverPrefix = "server"

data Server = Server {
    serverId             :: ServerId
  , serverName           :: ServerName
  , serverOrganization   :: OrganizationId
  , serverImage          :: Image
  , serverCommercialType :: CommercialType
  , serverTags           :: Tags
  , serverEnableIpv6     :: Maybe Bool
  , serverBootscript     :: Maybe BootScript
  , serverPrivateIp      :: Maybe Text
  , serverPublicIp       :: Maybe IpRef
  , serverState          :: ServerState
  , serverVolumes        :: HM.HashMap Int Volume
} deriving (Show, Eq, Generic)

instance FromJSON Server where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverPrefix) }

instance ToJSON Server where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length serverPrefix) }

newtype Servers = Servers { servers :: [Server] }
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Servers


newtype ServerResult = ServerResult { server :: Server }
  deriving (Show, Eq, Generic)

instance FromJSON ServerResult


serverCreatePrefix :: String
serverCreatePrefix = "serverCreate"

data ServerCreate = ServerCreate {
    serverCreateOrganization   :: OrganizationId
  , serverCreateName           :: ServerName
  , serverCreateCommercialType :: CommercialType
  , serverCreateImage          :: ImageId
  , serverCreateTags           :: Tags
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

newtype BootScriptId = BootScriptId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


bootScriptPrefix :: String
bootScriptPrefix = "bootScript"

data BootScript = BootScript {
    bootScriptId           :: BootScriptId
  , bootScriptKernel       :: Text
  , bootScriptInitrd       :: Text
  , bootScriptBootcmdargs  :: Text
  , bootScriptArchitecture :: Text
  , bootScriptTitle        :: Text
  , bootScriptDtb          :: Text
  , bootScriptOrganization :: OrganizationId
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
                    | ARM64_2GB
                    | ARM64_4GB
                    | ARM64_8GB
                    | ARM64_16GB
                    | ARM64_32GB
                    | ARM64_64GB
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
      "ARM64_2GB"   -> pure ARM64_2GB
      "ARM64-4GB"   -> pure ARM64_4GB
      "ARM64-8GB"   -> pure ARM64_8GB
      "ARM64-16GB"  -> pure ARM64_16GB
      "ARM64-32GB"  -> pure ARM64_32GB
      "ARM64-64GB"  -> pure ARM64_64GB
      "ARM64-128GB" -> pure ARM64_128GB
      _             -> fail $ "Unknown commercial_type: " ++ (unpack t)

instance ToJSON CommercialType where
  toJSON ct = case ct of
    ARM64_2GB   -> String "ARM64-128GB"
    ARM64_4GB   -> String "ARM64-128GB"
    ARM64_8GB   -> String "ARM64-128GB"
    ARM64_16GB  -> String "ARM64-128GB"
    ARM64_32GB  -> String "ARM64-128GB"
    ARM64_64GB  -> String "ARM64-128GB"
    ARM64_128GB -> String "ARM64-128GB"
    ct'         -> String . pack . show $ ct'

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
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Actions


newtype ActionRequest = ActionRequest { action :: Action }
  deriving (Show, Eq, Generic)

instance ToJSON ActionRequest


newtype TaskId = TaskId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


taskPrefix :: String
taskPrefix = "task"

data Task = Task {
    taskId          :: TaskId
  , taskDescription :: Text
  , taskHrefFrom    :: Text
  , taskProgress    :: Text
  , taskStatus      :: Text
} deriving (Show, Eq, Generic)

instance FromJSON Task where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length taskPrefix) }


data ActionResponse = ActionResponse { task :: Task }
  deriving (Show, Eq, Generic)

instance FromJSON ActionResponse


-------------------------------------------------------------------------------

newtype OrganizationId = OrganizationId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

newtype OrganizationName = OrganizationName Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

organizationPrefix :: String
organizationPrefix = "organization"

data Organization = Organization {
    organizationId    :: OrganizationId
  , organizationName  :: OrganizationName
  , organizationUsers :: [User]
} deriving (Show, Eq, Generic)

instance FromJSON Organization where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length organizationPrefix) }


newtype Organizations = Organizations { organizations :: [Organization] }
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Organizations


organizationRefPrefix :: String
organizationRefPrefix = "organizationRef"

data OrganizationRef = OrganizationRef {
    organizationRefId   :: OrganizationId
  , organizationRefName :: OrganizationName
} deriving (Show, Eq, Generic)

instance FromJSON OrganizationRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length organizationRefPrefix) }

instance ToJSON OrganizationRef where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length organizationRefPrefix) }


-------------------------------------------------------------------------------

newtype ImageId = ImageId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

newtype ImageName = ImageName Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

imagePrefix :: String
imagePrefix = "image"

data Image = Image {
    imageId               :: ImageId
  , imageName             :: ImageName
  , imageOrganization     :: OrganizationId
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


newtype Images = Images { images :: [Image] }
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Images


newtype ImageResult = ImageResult { image :: Image }
  deriving (Show, Eq, Generic)

instance FromJSON ImageResult


imageCreatePrefix :: String
imageCreatePrefix = "imageCreate"

data ImageCreate = ImageCreate {
    imageCreateOrganization :: OrganizationId
  , imageCreateArch         :: Text
  , imageCreateName         :: ImageName
  , imageCreateRootVolume   :: VolumeId
} deriving (Show, Eq, Generic)

instance ToJSON ImageCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length imageCreatePrefix) }


-------------------------------------------------------------------------------

newtype UserId = UserId Text
  deriving (Show, Eq, Generic, FromJSON, ToHttpApiData)

newtype UserEmail = UserEmail Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype UserFirstName = UserFirstName Text
  deriving (Show, Eq, Generic, FromJSON)

newtype UserLastName = UserLastName Text
  deriving (Show, Eq, Generic, FromJSON)

newtype UserFullName = UserFullName Text
  deriving (Show, Eq, Generic, FromJSON)


userPrefix :: String
userPrefix = "user"

data User = User {
    userId            :: UserId
  , userEmail         :: UserEmail
  , userFirstname     :: UserFirstName
  , userLastname      :: UserLastName
  , userFullname      :: UserFullName
  , userOrganizations :: Maybe [OrganizationRef]
  , userRoles         :: Maybe [Role]
  , userSshPublicKeys :: Maybe [SshPublicKey]
} deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length userPrefix) }


newtype UserResult = UserResult { user :: User }
  deriving (Show, Eq, Generic)

instance FromJSON UserResult


sshPublicKeyPrefix :: String
sshPublicKeyPrefix = "sshPublicKey"

data SshPublicKey = SshPublicKey {
    sshPublicKeyKey         :: Text
  , sshPublicKeyFingerprint :: Text
} deriving (Show, Eq, Generic)

instance FromJSON SshPublicKey where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length sshPublicKeyPrefix) }

-------------------------------------------------------------------------------


data RoleType = Manager
              | Admin
  deriving (Show, Eq, Generic)

instance FromJSON RoleType where
  parseJSON = withText "role type" $ \t ->
    case t of
      "manager" -> pure Manager
      "admin"   -> pure Admin
      _         -> fail $ "Unknown role type: " ++ show t

instance ToJSON RoleType where
  toJSON = String . pack . map toLower . show

rolePrefix :: String
rolePrefix = "role"

data Role = Role {
    roleOrganization :: Maybe OrganizationRef
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

newtype VolumeId = VolumeId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

newtype VolumeName = VolumeName Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)


volumePrefix :: String
volumePrefix = "volume"

data Volume = Volume {
      volumeId               :: VolumeId
    , volumeName             :: VolumeName
    , volumeOrganization     :: OrganizationId
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


newtype Volumes = Volumes { volumes :: [Volume] }
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Volumes


newtype VolumeResult = VolumeResult { volume :: Volume }
  deriving (Show, Eq, Generic)

instance FromJSON VolumeResult


volumeCreatePrefix :: String
volumeCreatePrefix = "volumeCreate"

data VolumeCreate = VolumeCreate {
    volumeCreateName         :: VolumeName
  , volumeCreateOrganization :: OrganizationId
  , volumeCreate             :: Int
  , volumeCreateVolumeType   :: Text
} deriving (Show, Eq, Generic)

instance ToJSON VolumeCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length volumeCreatePrefix) }


volumeRefPrefix :: String
volumeRefPrefix = "volumeRef"

data VolumeRef = VolumeRef {
    volumeRefName :: VolumeName
  , volumeRefId   :: VolumeId
} deriving (Show, Eq, Generic)

instance FromJSON VolumeRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length volumeRefPrefix) }

instance ToJSON VolumeRef where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length volumeRefPrefix) }


-------------------------------------------------------------------------------

newtype IpId = IpId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

ipPrefix :: String
ipPrefix = "ip"

data Ip = Ip {
    ipId           :: IpId
  , ipAddress      :: Text
  , ipOrganization :: OrganizationId
  , ipServer       :: Maybe ServerRef
} deriving (Show, Eq, Generic)

instance FromJSON Ip where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length ipPrefix) }

instance ToJSON Ip where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length ipPrefix) }

newtype Ips = Ips { ips :: [Ip] }
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Ips


newtype IpResult = IpResult { ip :: Ip }
  deriving (Show, Eq, Generic)

instance FromJSON IpResult


ipCreatePrefix :: String
ipCreatePrefix = "ipCreate"

data IpCreate = IpCreate {
    ipCreateOrganization :: OrganizationId
} deriving (Show, Eq, Generic)

instance ToJSON IpCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length ipCreatePrefix) }


ipRefPrefix :: String
ipRefPrefix = "ipRef"

data IpRef = IpRef {
    ipRefId      :: IpId
  , ipRefAddress :: Text
  , ipRefDynamic :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON IpRef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length ipRefPrefix) }

instance ToJSON IpRef where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length ipRefPrefix) }


-------------------------------------------------------------------------------

newtype SecurityGroupId = SecurityGroupId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

newtype SecurityGroupName = SecurityGroupName Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


securityGroupPrefix :: String
securityGroupPrefix = "securityGroup"

data SecurityGroup = SecurityGroup {
    securityGroupId                    :: SecurityGroupId
  , securityGroupName                  :: SecurityGroupName
  , securityGroupOrganization          :: OrganizationId
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
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON SecurityGroups where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }


newtype SecurityGroupResult = SecurityGroupResult { securityGroup :: SecurityGroup }
  deriving (Show, Eq, Generic)

instance FromJSON SecurityGroupResult where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }


securityGroupCreate :: String
securityGroupCreate = "securityGroupCreate"

data SecurityGroupCreate = SecurityGroupCreate {
    securityGroupCreateOrganization :: OrganizationId
  , securityGroupCreateName         :: SecurityGroupName
  , securityGroupCreateDescription  :: Text
} deriving (Show, Eq, Generic)

instance ToJSON SecurityGroupCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length securityGroupCreate) }


-------------------------------------------------------------------------------


newtype SnapshotId = SnapshotId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)

newtype SnapshotName = SnapshotName Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


snapshotPrefix :: String
snapshotPrefix = "snapshot"

data Snapshot = Snapshot {
    snapshotId           :: SnapshotId
  , snapshotName         :: SnapshotName
  , snapshotOrganization :: OrganizationId
  , snapshotBaseVolume   :: Maybe VolumeRef
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
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Snapshots


newtype SnapshotResult = SnapshotResult { snapshot :: Snapshot }
  deriving (Show, Eq, Generic)

instance FromJSON SnapshotResult


snapshotCreate :: String
snapshotCreate = "snapshotCreate"

data SnapshotCreate = SnapshotCreate {
    snapshotCreateName         :: SnapshotName
  , snapshotCreateOrganization :: OrganizationId
  , snapshotCreateVolumeId     :: VolumeId
} deriving (Show, Eq, Generic)

instance ToJSON SnapshotCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length snapshotCreate) }


-------------------------------------------------------------------------------


newtype TokenId = TokenId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)


tokenPrefix :: String
tokenPrefix = "token"

data Token = Token {
    tokenId                :: TokenId
  , tokenCreationDate      :: UTCTime
  , tokenExpires           :: Maybe UTCTime
  , tokenInheritsUserPerms :: Bool
  , tokenRoles             :: Role
} deriving (Show, Eq, Generic)

instance FromJSON Token where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length tokenPrefix) }

instance ToJSON Token where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length tokenPrefix) }


newtype Tokens = Tokens { tokens :: [Token] }
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON Tokens


newtype TokenResult = TokenResult { token :: Token }
  deriving (Show, Eq, Generic)

instance FromJSON TokenResult


tokenCreatePrefix :: String
tokenCreatePrefix = "tokenCreate"

data TokenCreate = TokenCreate {
    tokenCreateEmail    :: UserEmail
  , tokenCreatePassword :: Text
  , tokenCreateExpires  :: Bool
} deriving (Show, Eq, Generic)

instance ToJSON TokenCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase . drop (length tokenCreatePrefix) }


-------------------------------------------------------------------------------

newtype SecurityRuleId = SecurityRuleId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData)


securityRulePrefix :: String
securityRulePrefix = "securityRule"

data SecurityRule = SecurityRule {
    securityRuleId           :: SecurityRuleId
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

newtype SecurityRules = SecurityRules { rules :: [SecurityRule] }
  deriving (Show, Eq, Generic, Monoid)

instance FromJSON SecurityRules where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }


data SecurityRuleResult = SecurityRuleResult { rule :: SecurityRule }
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
