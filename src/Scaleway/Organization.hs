{-# LANGUAGE DeriveGeneric #-}

module Scaleway.Organization
    ( OrganizationId(..)
    ) where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

newtype OrganizationId = OrganizationId Text deriving (Show, Eq, Generic)

instance FromJSON OrganizationId
instance ToJSON OrganizationId
