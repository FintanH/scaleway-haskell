{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Network.Wreq
import           Scaleway

main :: IO ()
main = do
  servers <- listServers' "<my test token>" "par1" "1" "100"
  print servers
