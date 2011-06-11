module Types.EntityId ( EntityId
                      , getId
                      , getIdFromString
                      ) where

import System.Random (randomIO)

newtype EntityId = EntityId String
                  deriving (Show, Eq)

getId :: IO EntityId
getId =
    do  int <- randomIO
        return . EntityId . show $ (int :: Int)

getIdFromString :: String -> EntityId
getIdFromString str = EntityId str
