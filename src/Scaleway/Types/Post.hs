{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Scaleway.Types.Post
    ( Server(..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON, defaultOptions,
                                            genericToJSON, toJSON)
import           Data.Aeson.Types          (Options (..))
import           Data.Text                 (Text)
import           GHC.Generics
import           Scaleway.Internal.Utility (jsonSnakeCaseWithModifier)
import           Scaleway.Types.Internal   (CommercialType, ImageId,
                                            OrganizationId, Tag)

data Server = Server {
    name           :: Text
  , organization   :: OrganizationId
  , image          :: ImageId
  , commercialType :: CommercialType
  , tags           :: [Tag]
  , enableIpv6     :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Server
instance ToJSON Server where
  toJSON = jsonSnakeCaseWithModifier modifier . genericToJSON defaultOptions
    where
      modifier "enable_ipv_6" = "enable_ipv6"
      modifier x              = x
