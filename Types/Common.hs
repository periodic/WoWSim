{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable #-}
module Types.Common  where

import DisEvSim (Time, DTime, Sim)
import Data.Map (Map(..))
import Control.Monad.Reader

-- For Data/Typable
import Data.Data
import Data.Typeable

-- For labels
import Language.Haskell.TH
import Data.Record.Label
import System.Random
import qualified Control.Category as Cat

a <.> b = (Cat..) a b
mapFst f (a, b) = (f a, b)
mapSnd f (a, b) = (a, f b)

-- * Config type

data SimConfig = SimConfig {
                           } deriving (Show)



-- * General types

data ActionState = ActionState { _actionSource :: Entity
                               , _actionTarget :: Entity
                               } deriving (Show)

type Action = ReaderT ActionState (Sim World Event)

type Health = Integer
type Damage = Integer

newtype EntityId = EntityId String
                   deriving (Eq, Ord)
instance Show EntityId where
    show (EntityId name) = name

type EntityMap = Map EntityId Entity

data Entity = Entity { _eID        :: !EntityId
                     , _eTarget    :: !EntityId
                     , _eHealth    :: !Health
                     , _eGlobalCD  :: !Time
                     , _eCooldowns :: Map String Time
                     , _eStats     :: Stats
                     } deriving (Show)

data World = World { _wEntities :: EntityMap
                   , _wGen      :: !StdGen
                   } deriving (Show)

data Event = EvSimStart
           | EvGcdEnd EntityId
           | EvCooldownExpire EntityId AbilityId
           -- Autoattack start/stop ready
           | EvAutoAttackStart EntityId
           | EvAutoAttackStop  EntityId
           | EvAutoAttackReady EntityId
           | EvHit   EntityId EntityId AbilityId Damage
           | EvCrit  EntityId EntityId AbilityId Damage
           | EvDodge EntityId EntityId AbilityId
           | EvParry EntityId EntityId AbilityId
           | EvMiss  EntityId EntityId AbilityId
           --deriving (Show)

type AbilityMap = Map AbilityId Ability
type AbilityId = String
data Ability = Ability { _abilName       :: String
                       , _abilCooldown   :: Maybe DTime
                       , _abilTriggerGCD :: Bool
                       , _abilAction     :: Action ()
                       }
-- * Function types

type HitFunc = Stats -> Stats -> Damage -> Damage -> StdGen -> (AttackResult, StdGen)


-- * Various Enumerations

data Class     = Warrior
               | Paladin
               | DeathKnight
               | Hunter
               | Shaman
               | Druid
               | Rogue
               | Mage
               | Priest
               | Warlock
                 deriving (Show, Eq, Typeable, Data)

data PowerType = Mana
               | Rage
               | Energy
               | Focus
               | RunicPower
                 deriving (Show, Eq, Typeable, Data)

data RuneType = BloodRune
              | UnholyRune
              | FrostRune
              | DeathRune
                deriving (Show, Eq, Typeable, Data)

data SpellSchool = Arcane
                 | Fire
                 | Frost
                 | Nature
                 | Holy
                 | Shadow
                 | Physical
                   deriving (Show, Eq, Typeable, Data)

data AuraType   = Buff 
                | DebuffMagic
                | DebuffCurse
                | DebuffPoison
                | DebuffDisease
                | DebuffBleed
                | DebuffOther
                deriving (Show, Eq, Typeable, Data)

data AttackResult = ResultHit Damage
                  | ResultCrit Damage
                  | ResultDodge
                  | ResultParry
                  | ResultMiss
                    deriving (Eq, Show, Typeable, Data)

-- * Stats

data Stats = Stats { _level              :: Integer
                   , _characterClass     :: Class
                   -- Ability Scores
                   , _strength           :: Integer
                   , _agility            :: Integer
                   , _stamina            :: Integer
                   , _intellect          :: Integer
                   , _spirit             :: Integer
                   -- ratings
                   , _critRating         :: Integer
                   , _spellCrit          :: Float
                   , _meleeCrit          :: Float
                   , _hasteRating        :: Integer
                   , _meleeHaste         :: Float
                   , _spellHaste         :: Float
                   , _hitRating          :: Integer
                   , _spellHit           :: Float
                   , _meleeHit           :: Float
                   , _masteryRating      :: Integer
                   , _mastery            :: Float
                   -- Melee stats
                   , _attackPower        :: Integer
                   , _armorPen           :: Integer
                   , _armorIgnored       :: Float
                   -- Weapon stats
                   {-
                   , _mhWeaponSpeed      :: DTime
                   , _mhWeaponMinDamage  :: Health
                   , _mhWeaponMaxDamage  :: Health
                   , _ohWeaponSpeed      :: DTime
                   , _ohWeaponMinDamage  :: Health
                   , _ohWeaponMaxDamage  :: Health
                   -}
                   , _weaponSpeed        :: DTime
                   , _weaponMinDamage    :: Damage
                   , _weaponMaxDamage    :: Damage
                   -- Magic stats
                   , _arcaneSpellDamage  :: Integer
                   , _fireSpellDamage    :: Integer
                   , _frostSpellDamage   :: Integer
                   , _holySpellDamage    :: Integer
                   , _natureSpellDamage  :: Integer
                   , _shadowSpellDamage  :: Integer

                   , _arcaneDamageMult   :: Float
                   , _fireDamageMult     :: Float
                   , _frostDamageMult    :: Float
                   , _holyDamageMult     :: Float
                   , _natureDamageMult   :: Float
                   , _shadowDamageMult   :: Float

                   , _spellDamageMult    :: Float

                   , _spellCostMultOff   :: Float
                   , _spellCostMultDef   :: Float
                   -- Defenses
                   , _armor              :: Integer
                   , _damageReduction    :: Float
                   , _parryRating        :: Integer
                   , _parryChance        :: Float
                   , _dodgeRating        :: Integer
                   , _dodgeChance        :: Float
                   , _defenseRating      :: Integer
                   , _missChance         :: Float
                   , _critReduction      :: Float
                   -- Power Source
                   , _powerType          :: PowerType
                   , _powerMax           :: Integer
                   , _powerRegen         :: Float
                   -- Health
                   , _healthMax          :: Health
                   -- Other Status
                   , _gcd                :: Float
                   , _physCritMult       :: Float
                   , _physMult           :: Float
                   , _spellCritMult      :: Float
                   , _spellMult          :: Float
                   } deriving (Show)

$(mkLabels [''Ability, ''Action, ''ActionState, ''Entity, ''World, ''Stats])
