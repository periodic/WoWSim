module Types.Entity ( Entity(..)
                    , getId
                    , onGCD
                    , onCooldown
                    , addCooldown
                    , makeEntity
                    ) where

import DisEvSim (DTime, Time)
import Types.Common
import Types.EntityId
import Data.Map

{-
data Entity = Entity { eID        :: !EntityId
                     , eHealth    :: !Health
                     , eGlobalCD  :: !Time
                     , eCooldowns :: !Map String Time
                     } deriving (Show)
-}

makeEntity name = Entity (getIdFromString name) 0 0 empty

onGCD :: Entity -> Time -> Bool
onGCD e t = eGlobalCD e > t

onCooldown :: Entity -> String -> Time -> Bool
onCooldown e name t = 
    case Data.Map.lookup name (eCooldowns e) of
        Nothing -> False
        Just t' -> t' > t

addCooldown :: Entity -> String -> Time -> Entity
addCooldown e name t =
    let cds = insert name t (eCooldowns e)
     in e { eCooldowns = cds }
