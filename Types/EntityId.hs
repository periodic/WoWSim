module Types.EntityId ( EntityId
                      , getId
                      , getIdFromString
                      ) where

import Types.Common
import System.Random (randomIO)

getId :: IO EntityId
getId =
    do  int <- randomIO
        return . EntityId . show $ (int :: Int)

getIdFromString :: String -> EntityId
getIdFromString str = EntityId str
