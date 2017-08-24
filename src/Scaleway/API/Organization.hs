{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Organization
    ( OrganizationAPI
    , getOrganizationsM
    ) where

import           Data.Proxy        (Proxy (..))
import           Data.Text         (Text)
import           Scaleway.API.Core (Page, PerPage, ScalewayAuthToken,
                                    XAuthToken)
import           Scaleway.Types    (Organizations)
import           Servant.API       ((:<|>) (..), (:>), Get, JSON, Post, Put,
                                    QueryParam)
import           Servant.Client    (ClientM, client)

type OrganizationAPI =
       "organizations" :> ScalewayAuthToken
                       :> QueryParam "per_page" PerPage
                       :> QueryParam "page" Page
                       :> Get '[JSON] Organizations

serverAPI :: Proxy OrganizationAPI
serverAPI = Proxy

getOrganizationsM :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Organizations
getOrganizationsM = client serverAPI
