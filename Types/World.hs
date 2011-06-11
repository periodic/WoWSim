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
                   , Event(..)
                   , Ability(..)
                   ) where

import Types.Common
import Types.Entity

{-
data World = World { player :: !Entity
                   , target :: !Entity
                   } deriving (Show)
-}
