{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Scaleway.API.Core
    ( XAuthToken (..)
    , ScalewayAuthToken
    , ScalewayClient
    , ScalewayAccountClient (..)
    , ScalewayComputeClient (..)
    , PerPage
    , Page
    , ParamPage
    , ParamPerPage
    , scalewayGetListRequest
    , scalewayGetSingleRequest
    , scalewayPostRequest
    , scalewayPutRequest
    , scalewayDeleteRequest
    , runAccountClient
    , runComputeClient
    ) where

import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.Trans     (lift)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.String             (IsString)
import           Data.Text               (Text)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Scaleway.Types          (Region)
import           Servant.API             ((:>), Header, QueryParam,
                                          ToHttpApiData (toUrlPiece))
import           Servant.Client          (BaseUrl (..), ClientEnv (..), ClientM,
                                          Scheme (..), ServantError, runClientM)

newtype XAuthToken = XAuthToken Text
  deriving (Eq, Show, IsString, ToHttpApiData)

newtype PerPage = PerPage Int
  deriving (Eq, Show, Num, ToHttpApiData)

newtype Page = Page Int
  deriving (Eq, Show, Num, ToHttpApiData)

type ScalewayAuthToken = Header "X-Auth-Token" XAuthToken

type ParamPerPage = QueryParam "per_page" PerPage

type ParamPage = QueryParam "page" Page

type ScalewayClient resourceType a = ReaderT XAuthToken ClientM a

data Account
data Compute

newtype ScalewayAccountClient a = ScalewayAccount (ScalewayClient Account a)
newtype ScalewayComputeClient a = ScalewayCompute (ScalewayClient Compute a)

newtype HostPrefix = HostPrefix String

clientEnv :: HostPrefix -> IO ClientEnv
clientEnv (HostPrefix prefix) = do
  let host = prefix ++ ".scaleway.com"
  manager <- newTlsManager
  pure $ ClientEnv manager (BaseUrl Http host 80 "")

computeEnv :: Region -> IO ClientEnv
computeEnv region = clientEnv (HostPrefix $ "cp-" ++ show region)

accountEnv :: IO ClientEnv
accountEnv = clientEnv (HostPrefix "account")

runAccountClient :: ScalewayAccountClient a -> XAuthToken -> IO (Either ServantError a)
runAccountClient (ScalewayAccount c) token = do
  env <- accountEnv
  runClientM (runReaderT c token) env

runComputeClient :: ScalewayComputeClient a -> Region -> XAuthToken -> IO (Either ServantError a)
runComputeClient (ScalewayCompute c) r token = do
  env <- computeEnv r
  runClientM (runReaderT c token) env

scalewayGetListRequest :: FromJSON a
                       => (Maybe XAuthToken -> Maybe PerPage -> Maybe Page -> ClientM a)
                       -> Maybe PerPage -> Maybe Page -> ScalewayClient resourceType a
scalewayGetListRequest r perPage page = do
  token <- ask
  lift $ r (Just token) perPage page

scalewayGetSingleRequest :: FromJSON a
                         => (Maybe XAuthToken -> id -> ClientM a)
                         -> id -> ScalewayClient resourceType a
scalewayGetSingleRequest r resourceId = do
  token <- ask
  lift $ r (Just token) resourceId

scalewayPostRequest :: (ToJSON a, FromJSON b)
                    => (Maybe XAuthToken -> a -> ClientM b)
                    -> a -> ScalewayClient resourceType b
scalewayPostRequest r postData = do
  token <- ask
  lift $ r (Just token) postData

scalewayPutRequest :: (ToJSON a, FromJSON b)
                   => (Maybe XAuthToken -> id -> a -> ClientM b)
                   -> id -> a -> ScalewayClient resourceType b
scalewayPutRequest r resourceId body = do
  token <- ask
  lift $ r (Just token) resourceId body

scalewayDeleteRequest :: (Maybe XAuthToken -> id -> ClientM ())
                      -> id -> ScalewayClient resourceType ()
scalewayDeleteRequest r resourceId = do
  token <- ask
  lift $ r (Just token) resourceId
