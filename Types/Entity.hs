module Types.Entity ( Entity(..)
                    , getId
                    ) where

import DisEvSim (DTime)
import Types.Common
import Types.EntityId

data Entity = Entity { id     :: EntityId
                     , health :: Health
                     , gcd    :: DTime
                     } deriving (Show)

