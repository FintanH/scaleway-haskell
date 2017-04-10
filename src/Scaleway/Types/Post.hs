{-# LANGUAGE DeriveGeneric #-}

module Scaleway.Types.Post
    ( Server(..)
    ) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Scaleway.Types.Internal (OrganizationId, ImageId, CommercialType, Tag)
import Data.Text (Text)

data Server = Server {
    name :: Text
  , organization :: OrganizationId
  , imageId :: ImageId
  , commercialType :: CommercialType
  , tags :: [Tag]
  , enableIpv6 :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Server
instance ToJSON Server
