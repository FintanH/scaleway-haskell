{-# LANGUAGE OverloadedStrings #-}

module Scaleway.Network.Servers where

import           Control.Lens
import           Data.Aeson                (Value, eitherDecode, withObject,
                                            (.:))
import           Data.Aeson.Types          (parseEither, parseJSON)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Monoid               ((<>))
import           Network.Wreq              (Response, defaults, getWith,
                                            responseBody)
import           Scaleway.Internal.Request
import           Scaleway.Types.Types      (Server)

listServers' :: HeaderToken -> Region -> Page -> PerPage -> IO (Response ByteString)
listServers' headerToken region pageNumber nPerPage = do
  let url = unUrl (requestUrl region) <> "/servers"
      opts = defaults & (pageParam pageNumber) & (perPageParam nPerPage) & (scalewayHeader headerToken)
  getWith opts url

listServers :: HeaderToken -> Region -> Page -> PerPage -> IO (Either String [Server])
listServers headerToken region pageNumber nPerPage = do
  r <- listServers' headerToken region pageNumber nPerPage
  return $ parseEither parseServers =<< (eitherDecode $ r ^. responseBody :: Either String Value)
  where
    parseServers = withObject "servers" $ \o -> do
      serverList <- o .: "servers"
      traverse parseJSON serverList
