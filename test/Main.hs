{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Network.Wreq
import           Scaleway

main :: IO ()
main = do
  test <- listServers' "" "par1" "1" "1"
  print (test ^. responseBody)
  servers <- listServers "" "par1" "1" "1"
  print servers
