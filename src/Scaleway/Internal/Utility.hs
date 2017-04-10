module Scaleway.Internal.Utility
    ( jsonCamelCase
    , jsonSnakeCase
    , jsonSnakeCaseWithModifier
    ) where

import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import Cases (camelize, snakify)
import Data.Text (Text)

-- | Turn all keys in a JSON object to lowercase.
jsonCamelCase :: Value -> Value
jsonCamelCase (Object o) = Object . HM.fromList . map modifyKey . HM.toList $ o
  where modifyKey (key, val) = (camelize key, val)
jsonCamelCase x = x

jsonSnakeCase :: Value -> Value
jsonSnakeCase (Object o) = Object . HM.fromList . map modifyKey . HM.toList $ o
  where modifyKey (key, val) = (snakify key, val)
jsonSnakeCase x = x

jsonSnakeCaseWithModifier :: (Text -> Text) -> Value -> Value
jsonSnakeCaseWithModifier modifier (Object o) = Object . HM.fromList . map (modifyKey modifier) . HM.toList $ o
  where modifyKey modifier (key, val) = (modifier $ snakify key, val)
jsonSnakeCaseWithModifier _ x = x
