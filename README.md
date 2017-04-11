# Scaleway-Haskell
SDK for Scaleway API in Haskell

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

## Endpoints that do not have dual behaviour
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
