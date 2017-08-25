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
                                    XAuthToken, ParamPerPage, ParamPage,)
import           Scaleway.Types    (Organizations)
import           Servant.API       ((:<|>) (..), (:>), Get, JSON, Post, Put,
                                    QueryParam)
import           Servant.Client    (ClientM, client)

type OrganizationAPI =
       "organizations" :> ScalewayAuthToken
                       :> ParamPerPage
                       :> ParamPage
                       :> Get '[JSON] Organizations

organizationAPI :: Proxy OrganizationAPI
organizationAPI = Proxy

getOrganizationsM :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Organizations
getOrganizationsM = client organizationAPI
