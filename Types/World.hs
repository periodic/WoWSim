module Types.World ( World(..)
                   , Entity(..)
                   , getId
                   ) where

import Types.Entity

data World = World { player :: Entity
                   , target :: Entity
                   } deriving (Show)
