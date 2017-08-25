{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Snapshot
    ( SnapshotAPI
    , getSnapshotsM
    , getSnapshotM
    , putSnapshotM
    , postSnapshotM
    , deleteSnapshotM
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
                                    Snapshot, SnapshotCreate, SnapshotResult,
                                    Snapshots)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, QueryParam, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureSnapshotId = Capture "snapshotId" Text

type SnapshotAPI = "snapshots" :> (
       ScalewayAuthToken
    :> QueryParam "per_page" PerPage
    :> QueryParam "page" Page
    :> Get '[JSON] Snapshots

  :<|> ScalewayAuthToken
    :> CaptureSnapshotId
    :> Get '[JSON] Snapshot

  :<|> ScalewayAuthToken
    :> ReqBody '[JSON] SnapshotCreate
    :> Post '[JSON] SnapshotResult

  :<|> ScalewayAuthToken
    :> CaptureSnapshotId
    :> ReqBody '[JSON] Snapshot
    :> Put '[JSON] Snapshot

  :<|> ScalewayAuthToken
    :> CaptureSnapshotId
    :> Delete '[JSON] ()
  )

snapshotAPI :: Proxy SnapshotAPI
snapshotAPI = Proxy

getSnapshots_ :: Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM Snapshots
getSnapshot_ :: Maybe XAuthToken -> Text -> ClientM Snapshot
postSnapshot_ :: Maybe XAuthToken -> SnapshotCreate -> ClientM SnapshotResult
putSnapshot_ :: Maybe XAuthToken -> Text -> Snapshot -> ClientM Snapshot
deleteSnapshot_ :: Maybe XAuthToken -> Text -> ClientM ()
getSnapshots_
  :<|> getSnapshot_
  :<|> postSnapshot_
  :<|> putSnapshot_
  :<|> deleteSnapshot_ = client snapshotAPI

getSnapshotsM :: Maybe PerPage -> Maybe Page -> ScalewayClient Snapshots
getSnapshotsM = scalewayGetListRequest getSnapshots_

getSnapshotM :: Text -> ScalewayClient Snapshot
getSnapshotM = scalewayGetSingleRequest getSnapshot_

postSnapshotM :: SnapshotCreate -> ScalewayClient SnapshotResult
postSnapshotM = scalewayPostRequest postSnapshot_

putSnapshotM :: Text -> Snapshot -> ScalewayClient Snapshot
putSnapshotM = scalewayPutRequest putSnapshot_

deleteSnapshotM :: Text -> ScalewayClient ()
deleteSnapshotM = scalewayDeleteRequest deleteSnapshot_
