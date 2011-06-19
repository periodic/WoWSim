module Types.Entity ( Entity(..)
                    , getId
                    , entityOnGCD
                    , entityOnCooldown
                    , entityAddCooldown
                    , makeEntity
                    , addEntityList
                    , updateEntityList
                    ) where

import DisEvSim (DTime, Time)
import Data.Map

import Types.Common
import Types.EntityId
import Types.Stats

-- |Create a default entity.
makeEntity name = Entity { eID          = getIdFromString name
                         , eTarget      = getIdFromString ""
                         , eHealth      = 0 
                         , eGlobalCD    = 0 
                         , eCooldowns   = empty 
                         , eStats       = defaultStats
                         }

-- |Add an entity to the entity map.
addEntityList :: Entity -> EntityMap -> EntityMap
addEntityList e map = insert (eID e) e map

-- |Update an entity, replacing the old entity with the new one.
updateEntityList :: Entity -> EntityMap -> EntityMap
updateEntityList e map = insert (eID e) e map

-- |Test whether an entity is in GCD
entityOnGCD :: Entity -> Time -> Bool
entityOnGCD e t = eGlobalCD e > t

-- |Test whether the entity has the named ability on cooldown
entityOnCooldown :: Entity -> String -> Time -> Bool
entityOnCooldown e name t = 
    case Data.Map.lookup name (eCooldowns e) of
        Nothing -> False
        Just t' -> t' > t

-- |Add a cooldown to the entity's cooldown map.
entityAddCooldown :: Entity -> String -> Time -> Entity
entityAddCooldown e name t =
    let cds = insert name t (eCooldowns e)
     in e { eCooldowns = cds }
