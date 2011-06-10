module Types.Events where

import Types.Common
import Types.EventId

data Event = EvGcdEnd EntityId
           | EvSwingDamage EntityId EntityId Damage
           deriving (Show)
