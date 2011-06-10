module Types.EntityId ( EntityId
                      , getId
                      ) where

import System.Random (randomIO)

newtype EntityId = EntityId String
                  deriving (Show, Eq)

getId :: IO EntityId
getId =
    do  int <- randomIO
        return . EntityId . show $ (int :: Int)
