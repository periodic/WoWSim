module Types.Events where

import Types.Common
import Types.EntityId

data Event = EvGcdEnd EntityId
           | EvSwingDamage EntityId EntityId Damage
           deriving (Show)
