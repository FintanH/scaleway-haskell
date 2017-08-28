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
import           Scaleway.API.Core (Page, ParamPage, ParamPerPage, PerPage,
                                    ScalewayAuthToken,
                                    ScalewayComputeClient (..), XAuthToken,
                                    scalewayDeleteRequest,
                                    scalewayGetListRequest,
                                    scalewayGetSingleRequest,
                                    scalewayPostRequest, scalewayPutRequest)
import           Scaleway.Types    (Snapshot, SnapshotCreate, SnapshotId,
                                    SnapshotResult, Snapshots)
import           Servant.API       ((:<|>) (..), (:>), Capture, Delete, Get,
                                    JSON, Post, Put, ReqBody)
import           Servant.Client    (ClientM, client)

type CaptureSnapshotId = Capture "snapshotId" SnapshotId

type SnapshotAPI = "snapshots" :> (
       ScalewayAuthToken
    :> ParamPerPage
    :> ParamPage
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
getSnapshot_ :: Maybe XAuthToken -> SnapshotId -> ClientM Snapshot
postSnapshot_ :: Maybe XAuthToken -> SnapshotCreate -> ClientM SnapshotResult
putSnapshot_ :: Maybe XAuthToken -> SnapshotId -> Snapshot -> ClientM Snapshot
deleteSnapshot_ :: Maybe XAuthToken -> SnapshotId -> ClientM ()
getSnapshots_
  :<|> getSnapshot_
  :<|> postSnapshot_
  :<|> putSnapshot_
  :<|> deleteSnapshot_ = client snapshotAPI

getSnapshotsM :: Maybe PerPage -> Maybe Page -> ScalewayComputeClient Snapshots
getSnapshotsM perPage = ScalewayCompute . scalewayGetListRequest getSnapshots_ perPage

getSnapshotM :: SnapshotId -> ScalewayComputeClient Snapshot
getSnapshotM = ScalewayCompute . scalewayGetSingleRequest getSnapshot_

postSnapshotM :: SnapshotCreate -> ScalewayComputeClient SnapshotResult
postSnapshotM = ScalewayCompute . scalewayPostRequest postSnapshot_

putSnapshotM :: SnapshotId -> Snapshot -> ScalewayComputeClient Snapshot
putSnapshotM i = ScalewayCompute . scalewayPutRequest putSnapshot_ i

deleteSnapshotM :: SnapshotId -> ScalewayComputeClient ()
deleteSnapshotM = ScalewayCompute . scalewayDeleteRequest deleteSnapshot_
