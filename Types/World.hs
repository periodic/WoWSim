module Types.World (
                   -- From Types.Entity
                     getId
                   , onGCD
                   , onCooldown
                   , addCooldown
                   , makeEntity
                   -- From Types.Common
                   , Damage
                   , Health
                   , World(..)
                   , Entity(..)
                   , EntityId(..)
                   , Event(..)
                   , Ability(..)
                   , AbilityId
                   ) where

import Types.Common
import Types.Entity
import Types.Event

{-
data World = World { player :: !Entity
                   , target :: !Entity
                   } deriving (Show)
-}
