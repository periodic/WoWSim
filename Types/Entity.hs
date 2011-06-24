module Types.Entity ( Entity(..)
                    , getId
                    , entityOnGCD
                    , entityOnCooldown
                    , entityAddCooldown
                    , makeEntity
                    , addEntityList
                    , updateEntityList
                    , lookupEntityInList
                    , adjustEntityInList
                    ) where

import Prelude hiding (lookup)

import DisEvSim (DTime, Time)
import Data.Map
import Data.Record.Label
import Text.Printf

import Types.Common
import Types.EntityId
import Types.Stats
import Types.Handler
import Types.Aura


-- |Create a default entity.
makeEntity name targ ai = Entity { _eID          = getIdFromString name
                                 , _eTarget      = getIdFromString targ
                                 , _eHealth      = 0 
                                 , _eGlobalCD    = 0 
                                 , _eCast        = Nothing
                                 , _eCooldowns   = empty 
                                 , _eStats       = defaultStats
                                 , _eHandlers    = emptyHandlerList
                                 , _eAI          = ai
                                 , _eAuras       = emptyAuraMap
                                 }

-- |Add an entity to the entity map.
addEntityList :: Entity -> EntityMap -> EntityMap
addEntityList e map = insert (getL eID e) e map

-- |Update an entity, replacing the old entity with the new one.
updateEntityList :: Entity -> EntityMap -> EntityMap
updateEntityList e map = insert (getL eID e) e map

-- |Update an entity, replacing the old entity with the new one.
lookupEntityInList :: Entity -> EntityMap -> Maybe Entity
lookupEntityInList e map = lookup (getL eID e) map

-- |Adjust an entity in the list
adjustEntityInList :: (Entity -> Entity) -> EntityId -> EntityMap -> EntityMap
adjustEntityInList f eid = adjust f eid

-- |Test whether an entity is in GCD
entityOnGCD :: Time -> Entity -> Bool
entityOnGCD t e = getL eGlobalCD e > t

-- |Test whether the entity has the named ability on cooldown
entityOnCooldown :: String -> Time -> Entity -> Bool
entityOnCooldown name t e = 
    case Data.Map.lookup name (getL eCooldowns e) of
        Nothing -> False
        Just t' -> t' > t

-- |Add a cooldown to the entity's cooldown map.
entityAddCooldown :: String -> Time -> Entity -> Entity
entityAddCooldown name t e = modL eCooldowns (insert name t) e

-- ** Casting related functions
-- | Test whether the entity is casting
entityIsCasting :: Time -> Entity -> Bool
entityIsCasting t e = case getL eCast e of
    Just (_, ct)    -> ct > t
    Nothing         -> False

-- | Start the cast timer
entityStartCasting :: Time -> Ability -> Entity -> Entity
entityStartCasting t abil = setL eCast (Just (abil, t)) 
