{-# LANGUAGE OverloadedStrings #-}

module Scaleway.Internal.Request where

import           Control.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import           Network.Wreq         (Options, defaults, getWith, header,
                                       param)

newtype ScalewayUrl = ScalewayUrl { unUrl :: String } deriving (Show, Eq)
type HeaderToken = BS.ByteString

type Region = String
type Page = Text
type PerPage = Text

requestUrl :: Region -> ScalewayUrl
requestUrl region = ScalewayUrl $  "https://cp-" <> region <> ".scaleway.com"

scalewayHeader :: HeaderToken -> Options -> Options
scalewayHeader token = \options -> options & header "X-Auth-Token" .~ [token]

pageParam :: Page -> Options -> Options
pageParam pageNumber = param "page" .~ [pageNumber]

perPageParam :: PerPage -> Options -> Options
perPageParam n = param "per_page" .~ [n]
