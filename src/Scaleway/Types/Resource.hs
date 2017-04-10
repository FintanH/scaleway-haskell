{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Scaleway.Types.Resource
    ( GET(..)
    , ResourceType(..)
    , HasResourceId(..)
    , HasResourceName(..)
    , mkServer
    , mkVolume
    ) where

import           Data.Text               (Text)
import qualified Scaleway.Types.Get      as Get
import           Scaleway.Types.Internal (ImageId, OrganizationId,
                                          SecurityGroupId, SecurityRuleId,
                                          ServerId (..), SnapshotId, TokenId,
                                          UserId, VolumeId (..))

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
  getResourceName :: f -> a

data GET :: ResourceType -> * where
  ServerR :: ServerId -> GET ServerResource
  VolumeR :: VolumeId -> GET VolumeResource
  ImageR :: ImageId -> GET ImageResource
  OrgranizationR :: OrganizationId -> GET OrganizationResource
  UserR :: UserId -> GET UserResource
  SnapshotR :: SnapshotId -> GET SnapshotResource
  SecurityGroupR :: SecurityGroupId -> GET SecurityGroupResource
  SecurityRuleR :: SecurityRuleId -> GET SecurityRuleResource
  TokenR :: TokenId -> GET TokenResource

mkServer :: Text -> GET ServerResource
mkServer = ServerR . ServerId

mkVolume :: Text -> GET VolumeResource
mkVolume = VolumeR . VolumeId

instance HasResourceId (GET ServerResource) Text where
  getResourceId (ServerR (ServerId serverId)) = serverId

instance HasResourceName (GET ServerResource) String where
  getResourceName _ = "servers"

instance HasResourceId (GET VolumeResource) Text where
  getResourceId (VolumeR (VolumeId volumeId)) = volumeId

instance HasResourceName (GET VolumeResource) String where
  getResourceName _ = "volumes"
