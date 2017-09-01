{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Scaleway.API.SecurityGroup
    ( SecurityGroupAPI
    , getSecurityGroupsM
    , getSecurityGroupM
    , putSecurityGroupM
    , postSecurityGroupM
    , deleteSecurityGroupM
    , getSecurityRulesM
    , getSecurityRuleM
    , putSecurityRuleM
    , postSecurityRuleM
    , deleteSecurityRuleM
    ) where

import           Data.Proxy        (Proxy (..))
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAuthToken,
                                    ScalewayComputeClient (..), XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (SecurityGroup, SecurityGroupCreate,
                                    SecurityGroupId, SecurityGroupResult,
                                    SecurityGroups, SecurityRule,
                                    SecurityRuleCreate, SecurityRuleId,
                                    SecurityRuleResult, SecurityRules)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureSecurityGroupId = Capture "securityGroupId" SecurityGroupId
type CaptureSecurityRuleId = Capture "securityRuleId" SecurityRuleId

type SecurityGroupAPI = "security_groups" :> (
       ScalewayAuthToken
    :> ParamPerPage
    :> ParamPage
    :> Get '[JSON] SecurityGroups

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> Get '[JSON] SecurityGroup

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] SecurityGroupCreate
    :> Post '[JSON] SecurityGroupResult

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> ReqBody '[JSON] SecurityGroup
    :> Put '[JSON] SecurityGroupResult

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> Delete '[JSON] ()

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> "rules"
    :> ParamPerPage
    :> ParamPage
    :> Get '[JSON] SecurityRules

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> "rules"
    :> CaptureSecurityRuleId
    :> Get '[JSON] SecurityRuleResult

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> "rules"
    :> ReqBody '[JSON] SecurityRuleCreate
    :> Post '[JSON] SecurityRuleResult

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> "rules"
    :> CaptureSecurityRuleId
    :> ReqBody '[JSON] SecurityRule
    :> Put '[JSON] SecurityRuleResult

  :<|> ScalewayAuthToken
    :> CaptureSecurityGroupId
    :> "rules"
    :> CaptureSecurityRuleId
    :> Delete '[JSON] ()
  )

securityGroupAPI :: Proxy SecurityGroupAPI
securityGroupAPI = Proxy

getSecurityGroups_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM SecurityGroups
getSecurityGroup_ :: Maybe XAuthToken -> SecurityGroupId -> ClientM SecurityGroup
postSecurityGroup_ :: Maybe XAuthToken -> SecurityGroupCreate -> ClientM SecurityGroupResult
putSecurityGroup_ :: Maybe XAuthToken -> SecurityGroupId -> SecurityGroup -> ClientM SecurityGroupResult
deleteSecurityGroup_ :: Maybe XAuthToken -> SecurityGroupId -> ClientM ()

getSecurityRules_ :: Maybe XAuthToken -> SecurityGroupId -> Maybe PerPage -> Maybe Page -> ClientM SecurityRules
getSecurityRule_ :: Maybe XAuthToken -> SecurityGroupId -> SecurityRuleId -> ClientM SecurityRuleResult
postSecurityRule_ :: Maybe XAuthToken -> SecurityGroupId -> SecurityRuleCreate -> ClientM SecurityRuleResult
putSecurityRule_ :: Maybe XAuthToken -> SecurityGroupId -> SecurityRuleId -> SecurityRule -> ClientM SecurityRuleResult
deleteSecurityRule_ :: Maybe XAuthToken -> SecurityGroupId -> SecurityRuleId -> ClientM ()
getSecurityGroups_
  :<|> getSecurityGroup_
  :<|> postSecurityGroup_
  :<|> putSecurityGroup_
  :<|> deleteSecurityGroup_
  :<|> getSecurityRules_
  :<|> getSecurityRule_
  :<|> postSecurityRule_
  :<|> putSecurityRule_
  :<|> deleteSecurityRule_ = client securityGroupAPI

getSecurityGroupsM :: Maybe PerPage -> Maybe Page -> ScalewayComputeClient SecurityGroups
getSecurityGroupsM perPage = ScalewayCompute . scalewayGetListRequest getSecurityGroups_ perPage

getSecurityGroupM :: SecurityGroupId -> ScalewayComputeClient SecurityGroup
getSecurityGroupM = ScalewayCompute . scalewayGetSingleRequest getSecurityGroup_

postSecurityGroupM :: SecurityGroupCreate -> ScalewayComputeClient SecurityGroupResult
postSecurityGroupM = ScalewayCompute . scalewayPostRequest postSecurityGroup_

putSecurityGroupM :: SecurityGroupId -> SecurityGroup -> ScalewayComputeClient SecurityGroupResult
putSecurityGroupM i = ScalewayCompute . scalewayPutRequest putSecurityGroup_ i

deleteSecurityGroupM :: SecurityGroupId -> ScalewayComputeClient ()
deleteSecurityGroupM = ScalewayCompute . scalewayDeleteRequest deleteSecurityGroup_

getSecurityRulesM :: SecurityGroupId -> Maybe PerPage -> Maybe Page -> ScalewayComputeClient SecurityRules
getSecurityRulesM sgId perPage = ScalewayCompute . scalewayGetListRequest (`getSecurityRules_` sgId) perPage

getSecurityRuleM :: SecurityGroupId -> SecurityRuleId -> ScalewayComputeClient SecurityRuleResult
getSecurityRuleM sgId = ScalewayCompute . scalewayGetSingleRequest (`getSecurityRule_` sgId)

postSecurityRuleM :: SecurityGroupId -> SecurityRuleCreate -> ScalewayComputeClient SecurityRuleResult
postSecurityRuleM sgId = ScalewayCompute . scalewayPostRequest (`postSecurityRule_` sgId)

putSecurityRuleM :: SecurityGroupId -> SecurityRuleId -> SecurityRule -> ScalewayComputeClient SecurityRuleResult
putSecurityRuleM sgId i = ScalewayCompute . scalewayPutRequest (`putSecurityRule_` sgId) i

deleteSecurityRuleM :: SecurityGroupId -> SecurityRuleId -> ScalewayComputeClient ()
deleteSecurityRuleM sgId = ScalewayCompute . scalewayDeleteRequest (`deleteSecurityRule_` sgId)
