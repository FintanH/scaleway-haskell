
module Scaleway.Internal.Types.SecurityRule where

import Data.Text (Text, unpack)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?), Value(Object), withText)
import Data.Monoid ((<>))
import Scaleway.Internal.Types.ResourceId (SecurityRuleId, parseSecurityRuleId)

data Direction = Inbound
               | Outbound
               deriving (Show, Eq)

instance FromJSON Direction where
  parseJSON = withText "direction" $ \t ->
    case t of
      "inbound"  -> return Inbound
      "outbound" -> return Outbound
      _          -> fail (unpack $ "Could not parse direction: " <> t)

data Protocol = TCP
              | UDP
              deriving (Show, Eq)

instance FromJSON Protocol where
  parseJSON = withText "protocol" $ \t ->
    case t of
      "TCP"  -> return TCP
      "UDP"  -> return UDP
      _      -> fail (unpack $ "Could not parse protocol: " <> t)

data SecurityRuleBase = SecurityRuleBase {
    action    :: Text
  , direction :: Direction
  , protocol  :: Protocol
  , ipRange   :: Text
} deriving (Show, Eq)

type SecurityRulePost = SecurityRuleBase
data SecurityRuleGet = SecurityRuleGet {
    securityRule   :: SecurityRuleBase
  , securityRuleId :: SecurityRuleId
  , destPortFrom   :: Maybe Int
  , destPortTo     :: Maybe Int
  , position       :: Int
  , editable       :: Maybe Bool
} deriving (Show, Eq)

instance FromJSON SecurityRuleBase where
  parseJSON = withObject "security rule base" $ \o -> do
    action <- o .: "action"
    direction <- parseJSON =<< (o .: "direction")
    protocol <- parseJSON =<< (o .: "protocol")
    ipRange <- o .: "ip_range"
    return SecurityRuleBase {..}

instance FromJSON SecurityRuleGet where
  parseJSON = withObject "security rule GET response" $ \o -> do
    securityRule <- parseJSON (Object o)
    securityRuleId <- parseSecurityRuleId (Object o)
    destPortFrom <- o .:? "dest_port_from"
    destPortTo <- o .:? "dest_port_to"
    position <- o .: "position"
    editable <- o .:? "editable"
    return SecurityRuleGet {..}
