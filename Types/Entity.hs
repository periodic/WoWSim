module Types.Entity ( Entity(..)
                    , getId
                    , onGCD
                    ) where

import DisEvSim (DTime, Time)
import Types.Common
import Types.EntityId

data Entity = Entity { eID      :: EntityId
                     , health   :: Health
                     , globalCD :: Time
                     } deriving (Show)

onGCD :: Entity -> Time -> Bool
onGCD (Entity _ _ gcdT) t = gcdT > t
