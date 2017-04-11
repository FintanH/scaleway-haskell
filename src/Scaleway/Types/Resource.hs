{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds #-}

module Scaleway.Types.Resource
    ( HasResourceId(..)
    , HasResourceName(..)
    , GetServer
    , GetVolume
    , getServer
    , listServer
    , getVolume
    , listVolume
    -- , mkVolume
    -- , mkImage
    -- , mkOrganization
    -- , mkUser
    -- , mkSnapshot
    -- , mkSecurityGroup
    -- , mkSecurityRule
    -- , mkToken
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

data ScalewayGET i = ListResource
                   | GetResource i

data GET :: ScalewayGET i -> ResourceType -> * where
  GetServerR :: ServerId -> GET (GetResource ServerId) ServerResource
  ListServerR :: GET ListResource ServerResource
  GetVolumeR :: VolumeId -> GET (GetResource VolumeId) VolumeResource
  ListVolumeR :: GET ListResource VolumeResource
  GetImageR :: ImageId -> GET (GetResource ImageId) ImageResource
  ListImageR :: GET ListResource ImageResource
  GetOrganizationR :: OrganizationId -> GET (GetResource OrganizationId) OrganizationResource
  ListOrganizationR :: GET ListResource OrganizationResource
  GetUserR :: UserId -> GET (GetResource UserId) UserResource
  ListUserR :: GET ListResource UserResource
  GetSnapshotR :: SnapshotId -> GET (GetResource SnapshotId) SnapshotResource
  ListSnapshotR :: GET ListResource SnapshotResource
  GetSecurityGroupR :: SecurityGroupId -> GET (GetResource SecurityGroupId) SecurityGroupResource
  ListSecurityGroupR :: GET ListResource SecurityGroupResource
  GetSecurityRuleR :: SecurityRuleId -> GET (GetResource SecurityRuleId) SecurityRuleResource
  ListSecurityRuleR :: GET ListResource SecurityRuleResource
  GetTokenR :: TokenId -> GET (GetResource TokenId) TokenResource
  ListTokenR :: GET ListResource TokenResource

-- | Aliases for our GADT GetResource result types
type GetServer = GET (GetResource ServerId) ServerResource
type GetVolume = GET (GetResource VolumeId) VolumeResource
type GetImage = GET (GetResource ImageId) ImageResource
type GetOrganization = GET (GetResource OrganizationId) OrganizationResource
type GetUser = GET (GetResource UserId) UserResource
type GetSnapshot = GET (GetResource SnapshotId) SnapshotResource
type GetSecurityGroup = GET (GetResource SecurityGroupId) SecurityGroupResource
type GetSecurityRule = GET (GetResource SecurityRuleId) SecurityRuleResource
type GetToken = GET (GetResource TokenId) TokenResource

getServer :: Text -> GetServer
getServer = GetServerR . ServerId

listServer :: GET ListResource ServerResource
listServer = ListServerR

getVolume :: Text -> GetVolume
getVolume = GetVolumeR . VolumeId

listVolume :: GET ListResource VolumeResource
listVolume = ListVolumeR

mkImage :: Text -> GetImage
mkImage = GetImageR . ImageId

mkOrganization :: Text -> GetOrganization
mkOrganization = GetOrganizationR . OrganizationId

mkUser :: Text -> GetUser
mkUser = GetUserR . UserId

mkSnapshot :: Text -> GetSnapshot
mkSnapshot = GetSnapshotR . SnapshotId

mkSecurityGroup :: Text -> GetSecurityGroup
mkSecurityGroup = GetSecurityGroupR . SecurityGroupId

mkSecurityRule :: Text -> GetSecurityRule
mkSecurityRule = GetSecurityRuleR . SecurityRuleId

mkToken :: Text -> GetToken
mkToken = GetTokenR . TokenId

instance HasResourceId ServerId Text where
  getResourceId (ServerId serverId) = serverId

instance HasResourceId GetServer Text where
  getResourceId (GetServerR serverId) = getResourceId serverId

instance HasResourceName GetServer String where
  getResourceNamePlural   _ = "servers"
  getResourceNameSingular _ = "server"

instance HasResourceName (GET ListResource ServerResource) String where
  getResourceNamePlural   _ = "servers"
  getResourceNameSingular _ = "server"

instance HasResourceId VolumeId Text where
  getResourceId (VolumeId volumeId) = volumeId

instance HasResourceId GetVolume Text where
  getResourceId (GetVolumeR volumeId) = getResourceId volumeId

instance HasResourceName GetVolume String where
  getResourceNamePlural   _ = "volumes"
  getResourceNameSingular _ = "volume"

instance HasResourceName (GET ListResource VolumeResource) String where
  getResourceNamePlural   _ = "volumes"
  getResourceNameSingular _ = "volumes"

instance HasResourceId GetImage Text where
  getResourceId (GetImageR (ImageId imageId)) = imageId

instance HasResourceName GetImage String where
  getResourceNamePlural   _ = "images"
  getResourceNameSingular _ = "image"

instance HasResourceId GetOrganization Text where
  getResourceId (GetOrganizationR (OrganizationId organizationId)) = organizationId

instance HasResourceName GetOrganization String where
  getResourceNamePlural   _ = "organizations"
  getResourceNameSingular _ = "organization"

instance HasResourceId GetUser Text where
  getResourceId (GetUserR (UserId userId)) = userId

instance HasResourceName GetUser String where
  getResourceNamePlural   _ = "users"
  getResourceNameSingular _ = "user"

instance HasResourceId GetSnapshot Text where
  getResourceId (GetSnapshotR (SnapshotId snapshotId)) = snapshotId

instance HasResourceName GetSnapshot String where
  getResourceNamePlural   _ = "snapshots"
  getResourceNameSingular _ = "snapshot"

instance HasResourceId GetSecurityGroup Text where
  getResourceId (GetSecurityGroupR (SecurityGroupId securityGroupId)) = securityGroupId

instance HasResourceName GetSecurityGroup String where
  getResourceNamePlural   _ = "sercurity_groups"
  getResourceNameSingular _ = "sercurity_group"

instance HasResourceId GetSecurityRule Text where
  getResourceId (GetSecurityRuleR (SecurityRuleId securityRuleId)) = securityRuleId

instance HasResourceName GetSecurityRule String where
  getResourceNamePlural   _ = "security_rules"
  getResourceNameSingular _ = "security_rule"

instance HasResourceId GetToken Text where
  getResourceId (GetTokenR (TokenId tokenId)) = tokenId

instance HasResourceName GetToken String where
  getResourceNamePlural   _ = "tokens"
  getResourceNameSingular _ = "token"
