module Scaleway.Internal.Utility
    ( jsonCamelCase
    ) where

import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import Cases (camelize)

-- | Turn all keys in a JSON object to lowercase.
jsonCamelCase :: Value -> Value
jsonCamelCase (Object o) = Object . HM.fromList . map modifyKey . HM.toList $ o
  where modifyKey (key, val) = (camelize key, val)
jsonCamelCase x = x
