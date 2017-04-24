
module Scaleway.Internal.Types.SecurityGroup where

import Data.Text (Text)
import Data.Aeson (Value(Object), withObject, (.:))
import Data.Aeson.Types (Parser)

type SecurityGroupName = Text

data SecurityGroupBase org = SecurityGroupBase {
    name         :: SecurityGroupName
  , organization :: org
  , description  :: Text
} deriving (Show, Eq)

parseSecurityGroupBase :: (Value -> Parser org)
                       -> Value -> Parser (SecurityGroupBase org)
parseSecurityGroupBase orgParser = withObject "security group base" $ \o -> do
  name <- o .: "name"
  description <- o .: "description"
  organization <- orgParser (Object o)
  return SecurityGroupBase {..}
