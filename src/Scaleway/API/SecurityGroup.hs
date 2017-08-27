{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.SecurityGroup
    ( SecurityGroupAPI
    , getSecurityGroupsM
    , getSecurityGroupM
    , putSecurityGroupM
    , postSecurityGroupM
    , deleteSecurityGroupM
    ) where

import           Data.Proxy        (Proxy (..))
import           Data.Text         (Text)
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAuthToken, ScalewayClient,
                                    ScalewayComputeClient (..), XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions,
                                    SecurityGroup, SecurityGroupCreate,
                                    SecurityGroupId, SecurityGroupResult,
                                    SecurityGroups, SecurityRule,
                                    SecurityRuleCreate, SecurityRuleId,
                                    SecurityRuleResult, SecurityRules)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
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
    :> "rules"
    :> ParamPerPage
    :> ParamPage
    :> Get '[JSON] SecurityRules

  :<|> ScalewayAuthToken
    :> "rules"
    :> CaptureSecurityRuleId
    :> Get '[JSON] SecurityRule

  :<|> ScalewayAuthToken
    :> "rules"
    :> ReqBody '[JSON] SecurityRuleCreate
    :> Post '[JSON] SecurityRuleResult

  :<|> ScalewayAuthToken
    :> "rules"
    :> CaptureSecurityRuleId
    :> ReqBody '[JSON] SecurityRule
    :> Put '[JSON] SecurityRuleResult

  :<|> ScalewayAuthToken
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

getSecurityRules_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM SecurityRules
getSecurityRule_ :: Maybe XAuthToken -> SecurityRuleId -> ClientM SecurityRule
postSecurityRule_ :: Maybe XAuthToken -> SecurityRuleCreate -> ClientM SecurityRuleResult
putSecurityRule_ :: Maybe XAuthToken -> SecurityRuleId -> SecurityRule -> ClientM SecurityRuleResult
deleteSecurityRule_ :: Maybe XAuthToken -> SecurityRuleId -> ClientM ()
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

getSecurityRulesM :: Maybe PerPage -> Maybe Page -> ScalewayComputeClient SecurityRules
getSecurityRulesM perPage = ScalewayCompute . scalewayGetListRequest getSecurityRules_ perPage

getSecurityRuleM :: SecurityRuleId -> ScalewayComputeClient SecurityRule
getSecurityRuleM = ScalewayCompute . scalewayGetSingleRequest getSecurityRule_

postSecurityRuleM :: SecurityRuleCreate -> ScalewayComputeClient SecurityRuleResult
postSecurityRuleM = ScalewayCompute . scalewayPostRequest postSecurityRule_

putSecurityRuleM :: SecurityRuleId -> SecurityRule -> ScalewayComputeClient SecurityRuleResult
putSecurityRuleM i = ScalewayCompute . scalewayPutRequest putSecurityRule_ i

deleteSecurityRuleM :: SecurityRuleId -> ScalewayComputeClient ()
deleteSecurityRuleM = ScalewayCompute . scalewayDeleteRequest deleteSecurityRule_
