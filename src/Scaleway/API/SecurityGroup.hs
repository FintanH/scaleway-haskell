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
import           Scaleway.API.Core (Page, PerPage, ScalewayAuthToken,
                                    ScalewayClient, XAuthToken, ParamPerPage, ParamPage,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions,
                                    SecurityGroup, SecurityGroupCreate,
                                    SecurityGroupResult, SecurityGroups,
                                    SecurityRule, SecurityRuleCreate,
                                    SecurityRuleResult, SecurityRules, SecurityRuleId, SecurityGroupId)
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

getSecurityGroupsM :: Maybe PerPage -> Maybe Page -> ScalewayClient SecurityGroups
getSecurityGroupsM = scalewayGetListRequest getSecurityGroups_

getSecurityGroupM :: SecurityGroupId -> ScalewayClient SecurityGroup
getSecurityGroupM = scalewayGetSingleRequest getSecurityGroup_

postSecurityGroupM :: SecurityGroupCreate -> ScalewayClient SecurityGroupResult
postSecurityGroupM = scalewayPostRequest postSecurityGroup_

putSecurityGroupM :: SecurityGroupId -> SecurityGroup -> ScalewayClient SecurityGroupResult
putSecurityGroupM = scalewayPutRequest putSecurityGroup_

deleteSecurityGroupM :: SecurityGroupId -> ScalewayClient ()
deleteSecurityGroupM = scalewayDeleteRequest deleteSecurityGroup_

getSecurityRulesM :: Maybe PerPage -> Maybe Page -> ScalewayClient SecurityRules
getSecurityRulesM = scalewayGetListRequest getSecurityRules_

getSecurityRuleM :: SecurityRuleId -> ScalewayClient SecurityRule
getSecurityRuleM = scalewayGetSingleRequest getSecurityRule_

postSecurityRuleM :: SecurityRuleCreate -> ScalewayClient SecurityRuleResult
postSecurityRuleM = scalewayPostRequest postSecurityRule_

putSecurityRuleM :: SecurityRuleId -> SecurityRule -> ScalewayClient SecurityRuleResult
putSecurityRuleM = scalewayPutRequest putSecurityRule_

deleteSecurityRuleM :: SecurityRuleId -> ScalewayClient ()
deleteSecurityRuleM = scalewayDeleteRequest deleteSecurityRule_
