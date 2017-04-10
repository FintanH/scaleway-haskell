{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Scaleway.Types.Resource
    ( GET
    , ResourceType(..)
    , HasResourceId(..)
    , HasResourceName(..)
    , mkServer
    , mkVolume
    , mkImage
    , mkOrganization
    , mkUser
    , mkSnapshot
    , mkSecurityGroup
    , mkSecurityRule
    , mkToken
    ) where

import           Data.Text               (Text)
import qualified Scaleway.Types.Get      as Get
import           Scaleway.Types.Internal (ImageId (..), OrganizationId (..),
                                          SecurityGroupId (..),
                                          SecurityRuleId (..), ServerId (..),
                                          SnapshotId (..), TokenId (..),
                                          UserId (..), VolumeId (..))

data ResourceType = ServerResource
                  | VolumeResource
                  | ImageResource
                  | OrganizationResource
                  | UserResource
                  | SnapshotResource
                  | SecurityGroupResource
                  | SecurityRuleResource
                  | TokenResource

class HasResourceId f a | f -> a where
  getResourceId :: f -> a

class HasResourceName f a | f -> a where
  getResourceNamePlural   :: f -> a
  getResourceNameSingular :: f -> a

data GET :: ResourceType -> * where
  ServerR :: ServerId -> GET ServerResource
  VolumeR :: VolumeId -> GET VolumeResource
  ImageR :: ImageId -> GET ImageResource
  OrganizationR :: OrganizationId -> GET OrganizationResource
  UserR :: UserId -> GET UserResource
  SnapshotR :: SnapshotId -> GET SnapshotResource
  SecurityGroupR :: SecurityGroupId -> GET SecurityGroupResource
  SecurityRuleR :: SecurityRuleId -> GET SecurityRuleResource
  TokenR :: TokenId -> GET TokenResource

mkServer :: Text -> GET ServerResource
mkServer = ServerR . ServerId

mkVolume :: Text -> GET VolumeResource
mkVolume = VolumeR . VolumeId

mkImage :: Text -> GET ImageResource
mkImage = ImageR . ImageId

mkOrganization :: Text -> GET OrganizationResource
mkOrganization = OrganizationR . OrganizationId

mkUser :: Text -> GET UserResource
mkUser = UserR . UserId

mkSnapshot :: Text -> GET SnapshotResource
mkSnapshot = SnapshotR . SnapshotId

mkSecurityGroup :: Text -> GET SecurityGroupResource
mkSecurityGroup = SecurityGroupR . SecurityGroupId

mkSecurityRule :: Text -> GET SecurityRuleResource
mkSecurityRule = SecurityRuleR . SecurityRuleId

mkToken :: Text -> GET TokenResource
mkToken = TokenR . TokenId

instance HasResourceId (GET ServerResource) Text where
  getResourceId (ServerR (ServerId serverId)) = serverId

instance HasResourceName (GET ServerResource) String where
  getResourceNamePlural   _ = "servers"
  getResourceNameSingular _ = "server"

instance HasResourceId (GET VolumeResource) Text where
  getResourceId (VolumeR (VolumeId volumeId)) = volumeId

instance HasResourceName (GET VolumeResource) String where
  getResourceNamePlural   _ = "volumes"
  getResourceNameSingular _ = "volume"

instance HasResourceId (GET ImageResource) Text where
  getResourceId (ImageR (ImageId imageId)) = imageId

instance HasResourceName (GET ImageResource) String where
  getResourceNamePlural   _ = "images"
  getResourceNameSingular _ = "image"

instance HasResourceId (GET OrganizationResource) Text where
  getResourceId (OrganizationR (OrganizationId organizationId)) = organizationId

instance HasResourceName (GET OrganizationResource) String where
  getResourceNamePlural   _ = "organizations"
  getResourceNameSingular _ = "organization"

instance HasResourceId (GET UserResource) Text where
  getResourceId (UserR (UserId userId)) = userId

instance HasResourceName (GET UserResource) String where
  getResourceNamePlural   _ = "users"
  getResourceNameSingular _ = "user"

instance HasResourceId (GET SnapshotResource) Text where
  getResourceId (SnapshotR (SnapshotId snapshotId)) = snapshotId

instance HasResourceName (GET SnapshotResource) String where
  getResourceNamePlural   _ = "snapshots"
  getResourceNameSingular _ = "snapshot"

instance HasResourceId (GET SecurityGroupResource) Text where
  getResourceId (SecurityGroupR (SecurityGroupId securityGroupId)) = securityGroupId

instance HasResourceName (GET SecurityGroupResource) String where
  getResourceNamePlural   _ = "sercurity_groups"
  getResourceNameSingular _ = "sercurity_group"

instance HasResourceId (GET SecurityRuleResource) Text where
  getResourceId (SecurityRuleR (SecurityRuleId securityRuleId)) = securityRuleId

instance HasResourceName (GET SecurityRuleResource) String where
  getResourceNamePlural   _ = "security_rules"
  getResourceNameSingular _ = "security_rule"

instance HasResourceId (GET TokenResource) Text where
  getResourceId (TokenR (TokenId tokenId)) = tokenId

instance HasResourceName (GET TokenResource) String where
  getResourceNamePlural   _ = "tokens"
  getResourceNameSingular _ = "token"
