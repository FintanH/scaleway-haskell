{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scaleway.Types.Post
    ( Server(..)
    ) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, toJSON, genericToJSON, defaultOptions)
import Data.Aeson.Types (Options(..))
import Scaleway.Types.Internal (OrganizationId, ImageId, CommercialType, Tag)
import Data.Text (Text)
import Scaleway.Internal.Utility (jsonSnakeCaseWithModifier)

data Server = Server {
    name :: Text
  , organization :: OrganizationId
  , image :: ImageId
  , commercialType :: CommercialType
  , tags :: [Tag]
  , enableIpv6 :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Server
instance ToJSON Server where
  toJSON = jsonSnakeCaseWithModifier modifier . genericToJSON opts
    where
      opts = defaultOptions { fieldLabelModifier = modifyName }
      modifyName "enableIpv6" = "enable_ipv6"
      modifyName x = x

      modifier "enable_ipv_6" = "enable_ipv6"
      modifier x = x
