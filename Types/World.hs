module Types.World ( World(..)
                   -- From Types.Entity
                   , Entity(..)
                   , getId
                   , onGCD
                   -- From Types.Common
                   , Damage
                   , Health
                   ) where

import Types.Common
import Types.Entity

data World = World { player :: Entity
                   , target :: Entity
                   } deriving (Show)
