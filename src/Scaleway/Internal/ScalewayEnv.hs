module Scaleway.Internal.ScalewayEnv
    ( ScalewayEnv (..)
    ) where

import qualified Data.ByteString         as BS
import           Scaleway.Types.Internal (Region)

type HeaderToken = BS.ByteString

data ScalewayEnv = ScalewayEnv {
    authToken :: HeaderToken
  , region    :: Region
}
