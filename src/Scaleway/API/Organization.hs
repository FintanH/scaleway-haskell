{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Scaleway.API.Organization
    ( OrganizationAPI
    , getOrganizationsM
    ) where

import           Data.Proxy        (Proxy (..))
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAccountClient (..),
                                    ScalewayAuthToken, XAuthToken,
                                    scalewayGetListRequest)
import           Scaleway.Types    (Organizations)
import           Servant.API       ((:>), Get, JSON)
import           Servant.Client    (ClientM, client)

type OrganizationAPI =
       "organizations" :> ScalewayAuthToken
                       :> ParamPerPage
                       :> ParamPage
                       :> Get '[JSON] Organizations

organizationAPI :: Proxy OrganizationAPI
organizationAPI = Proxy

getOrganizations_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Organizations
getOrganizations_ = client organizationAPI

getOrganizationsM :: Maybe PerPage -> Maybe Page -> ScalewayAccountClient Organizations
getOrganizationsM perPage = ScalewayAccount . scalewayGetListRequest getOrganizations_ perPage
