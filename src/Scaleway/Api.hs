{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API
    ( module API
    , ScalewayAPI
    , scalewayAPI
    , Page (..)
    , PerPage (..)
    , XAuthToken (..)
    , ScalewayComputeClient
    , ScalewayAccountClient
    , runAccountClient
    , runComputeClient
    -- ^ re-export for convenience
    , ServantError
    )where

import           Data.Proxy                 (Proxy (..))
import           Scaleway.API.Core          (Page (..), PerPage (..),
                                             ScalewayAccountClient,
                                             ScalewayComputeClient,
                                             XAuthToken (..), runAccountClient,
                                             runComputeClient)
import           Scaleway.API.Image         as API
import           Scaleway.API.Ip            as API
import           Scaleway.API.Organization  as API
import           Scaleway.API.SecurityGroup as API
import           Scaleway.API.Server        as API
import           Scaleway.API.Snapshot      as API
import           Scaleway.API.Token         as API
import           Scaleway.API.User          as API
import           Scaleway.API.Volume        as API
import           Scaleway.Types             as API
import           Servant.API                ((:<|>) (..))
import           Servant.Client             (ServantError)

type ScalewayAPI =
       ServerAPI
  :<|> ImageAPI
  :<|> VolumeAPI
  :<|> IpAPI
  :<|> SnapshotAPI
  :<|> SecurityGroupAPI
  :<|> TokenAPI
  :<|> OrganizationAPI
  :<|> UserAPI

scalewayAPI :: Proxy ScalewayAPI
scalewayAPI = Proxy
