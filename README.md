# Scaleway-Haskell
SDK for Scaleway API in Haskell

# How to get started
Since the this is not uploaded on hackage, for the time being you can clone the git repo to get the code.

Example code:
``` haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Scaleway

main :: IO ()
main = do
  let headerToken = "x-auth-token"
  servers <- listServers headerToken Paris "1" "1" -- first page, one result in Paris
  print servers
```
To get more familiar with the functionality you can look at the sub-modules of Network. 

# TODO
* Add further functionality such as POSTs, PUTs and DELETEs
* Fix functionality in terms of API behaviour

# Notes

## Errors
When testing endpoint /organizations I was met with a 404:
``` json
{
  "message": "The requested URL was not found on the server.  If you entered the URL manually please check your spelling and try again.",
  "type": "404"
}
```

When testing endpoint /tokens I was met with a 404:
``` json
{
  "message": "The requested URL was not found on the server.  If you entered the URL manually please check your spelling and try again.",
  "type": "404"
}
```

When testing /security_groups/rules I was met with a 400:
``` json
{
  "fields": {
    "group_id": [
      "rules is not a valid UUID."
    ]
  },
  "message": "Validation Error",
  "type": "invalid_request_error"
}
```

## Endpoint Behaviours
Some endpoints support listing all resources, some only support getting a specific resource and some support both.
Here's the list:

* List all
  * Servers
  * Organizations
  * Volumes
  * Snapshots
  * Images
  * Security Groups
  * Tokens

* Get Specific Resource
  * Servers
  * Users
  * Volumes
  * Snapshots
  * Images
  * Security Groups
  * Tokens
