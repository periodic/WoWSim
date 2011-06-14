module Types.Common where

import DisEvSim (Time, DTime, Sim)
import Types.EntityId

import Data.Map (Map(..))

type Health = Integer
type Damage = Integer

type Action = Sim World Event ()

data Entity = Entity { eID        :: !EntityId
                     , eHealth    :: !Health
                     , eGlobalCD  :: !Time
                     , eCooldowns :: Map String Time
                     } deriving (Show)

data World = World { player :: !Entity
                   , target :: !Entity
                   } deriving (Show)

data Event = EvSimStart
           | EvGcdEnd EntityId
           | EvCooldownExpire
           | EvAutoAttackReady
           | EvSwingDamage EntityId EntityId Damage
           --deriving (Show)

type AbilityMap = Map String Ability

data Ability = Ability { abilName       :: String
                       , abilCooldown   :: Maybe DTime
                       , abilTriggerGCD :: Bool
                       , abilAction     :: Action
                       }
