{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scaleway.Internal.Types.ResourceId
    ( HasResourceId (..)
    , ResourceId (..)
    , ImageId
    , OrganizationId
    , SecurityGroupId
    , SecurityRuleId
    , ServerId
    , SnapshotId
    , TokenId
    , UserId
    , VolumeId
    , parseImageId
    , parseOrganizationId
    , parseSecurityGroupId
    , parseSecurityRuleId
    , parseServerId
    , parseSnapshotId
    , parseTokenId
    , parseUserId
    , parseVolumeId
    ) where

import           Control.Applicative (liftA2, (<|>))
import           Data.Aeson          (FromJSON, ToJSON, Value, toJSON,
                                      withObject, (.:))
import           Data.Aeson.Types    (Parser)
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unpack)

class HasResourceId f a where
  getResourceId :: f -> a

newtype ResourceId a = ResourceId { unResourceId :: a }
  deriving (Show, Eq)

instance HasResourceId (ResourceId a) a where
  getResourceId = unResourceId

parseResourceId :: (FromJSON a) => Text -> Value -> Parser (ResourceId a)
parseResourceId altKey = withObject (unpack $ altKey <> " id")
                                    (fmap ResourceId . liftA2 (<|>) (.: altKey) (.: "id"))

instance ToJSON a => ToJSON (ResourceId a) where
  toJSON = toJSON . unResourceId

type ImageId = ResourceId Text
type OrganizationId = ResourceId Text
type SecurityGroupId = ResourceId Text
type SecurityRuleId = ResourceId Text
type ServerId = ResourceId Text
type SnapshotId = ResourceId Text
type TokenId = ResourceId Text
type UserId = ResourceId Text
type VolumeId = ResourceId Text

parseImageId :: Value -> Parser ImageId
parseImageId = parseResourceId "image"

parseOrganizationId :: Value -> Parser OrganizationId
parseOrganizationId = parseResourceId "organization"

parseSecurityGroupId :: Value -> Parser SecurityGroupId
parseSecurityGroupId = parseResourceId "security_group"

parseSecurityRuleId :: Value -> Parser SecurityRuleId
parseSecurityRuleId = parseResourceId "security_rule"

parseServerId :: Value -> Parser ServerId
parseServerId = parseResourceId "server"

parseSnapshotId :: Value -> Parser SnapshotId
parseSnapshotId = parseResourceId "snapshot"

parseTokenId :: Value -> Parser TokenId
parseTokenId = parseResourceId "token"

parseUserId :: Value -> Parser UserId
parseUserId = parseResourceId "user"

parseVolumeId :: Value -> Parser VolumeId
parseVolumeId = parseResourceId "volume"
