{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API
    ( module API
    , ScalewayAPI
    )where

import           Data.Proxy          (Proxy (..))
import           Scaleway.API.Core
import           Scaleway.API.Server as API
import           Scaleway.Types      (Images, PublicIps, Server, Servers,
                                      Volumes)
import           Servant.API         ((:<|>) (..))

type ScalewayAPI = ServerAPI

scalewayAPI :: Proxy ScalewayAPI
scalewayAPI = Proxy
