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
                                    ScalewayClient, XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (ActionRequest, ActionResponse, Actions,
                                    SecurityGroup, SecurityGroupCreate,
                                    SecurityGroupResult, SecurityGroups,
                                    SecurityRule, SecurityRuleCreate,
                                    SecurityRuleResult, SecurityRules)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureSecurityGroupId = Capture "securityGroupId" Text
type CaptureSecurityRuleId = Capture "securityRuleId" Text

type SecurityGroupAPI = "security_groups" :> (
       ScalewayAuthToken
    :> QueryParam "per_page" PerPage
    :> QueryParam "page" Page
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
    :> QueryParam "per_page" PerPage
    :> QueryParam "page" Page
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
getSecurityGroup_ :: Maybe XAuthToken -> Text -> ClientM SecurityGroup
postSecurityGroup_ :: Maybe XAuthToken -> SecurityGroupCreate -> ClientM SecurityGroupResult
putSecurityGroup_ :: Maybe XAuthToken -> Text -> SecurityGroup -> ClientM SecurityGroupResult
deleteSecurityGroup_ :: Maybe XAuthToken -> Text -> ClientM ()

getSecurityRules_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM SecurityRules
getSecurityRule_ :: Maybe XAuthToken -> Text -> ClientM SecurityRule
postSecurityRule_ :: Maybe XAuthToken -> SecurityRuleCreate -> ClientM SecurityRuleResult
putSecurityRule_ :: Maybe XAuthToken -> Text -> SecurityRule -> ClientM SecurityRuleResult
deleteSecurityRule_ :: Maybe XAuthToken -> Text -> ClientM ()
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

getSecurityGroupM :: Text -> ScalewayClient SecurityGroup
getSecurityGroupM = scalewayGetSingleRequest getSecurityGroup_

postSecurityGroupM :: SecurityGroupCreate -> ScalewayClient SecurityGroupResult
postSecurityGroupM = scalewayPostRequest postSecurityGroup_

putSecurityGroupM :: Text -> SecurityGroup -> ScalewayClient SecurityGroupResult
putSecurityGroupM = scalewayPutRequest putSecurityGroup_

deleteSecurityGroupM :: Text -> ScalewayClient ()
deleteSecurityGroupM = scalewayDeleteRequest deleteSecurityGroup_

getSecurityRulesM :: Maybe PerPage -> Maybe Page -> ScalewayClient SecurityRules
getSecurityRulesM = scalewayGetListRequest getSecurityRules_

getSecurityRuleM :: Text -> ScalewayClient SecurityRule
getSecurityRuleM = scalewayGetSingleRequest getSecurityRule_

postSecurityRuleM :: SecurityRuleCreate -> ScalewayClient SecurityRuleResult
postSecurityRuleM = scalewayPostRequest postSecurityRule_

putSecurityRuleM :: Text -> SecurityRule -> ScalewayClient SecurityRuleResult
putSecurityRuleM = scalewayPutRequest putSecurityRule_

deleteSecurityRuleM :: Text -> ScalewayClient ()
deleteSecurityRuleM = scalewayDeleteRequest deleteSecurityRule_
