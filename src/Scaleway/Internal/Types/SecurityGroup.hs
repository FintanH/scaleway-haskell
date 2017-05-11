{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Scaleway.Internal.Types.SecurityGroup where

import Data.Text (Text)
import Data.Aeson (Value(Object), withObject, (.:))
import Data.Aeson.Types (Parser)
import Control.Lens (makeLenses)

type SecurityGroupName = Text

data SecurityGroupBase org = SecurityGroupBase {
    securityGroupBaseName         :: SecurityGroupName
  , securityGroupBaseOrganization :: org
  , securityGroupBaseDescription  :: Text
} deriving (Show, Eq)

makeLenses ''SecurityGroupBase

parseSecurityGroupBase :: (Value -> Parser org)
                       -> Value -> Parser (SecurityGroupBase org)
parseSecurityGroupBase orgParser = withObject "security group base" $ \o -> do
  securityGroupBaseName <- o .: "name"
  securityGroupBaseDescription <- o .: "description"
  securityGroupBaseOrganization <- orgParser (Object o)
  return SecurityGroupBase {..}
