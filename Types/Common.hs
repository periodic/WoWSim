{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable #-}
module Types.Common  where

import DisEvSim (Time, DTime, Sim)
import Data.Map (Map(..), keys, elems)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import System.Random

-- For Data/Typable
import Data.Data
import Data.Typeable

-- For shows
import Text.Printf (printf)

-- For labels
import Language.Haskell.TH
import Data.Record.Label

import qualified Control.Category as Cat

a <.> b = (Cat..) a b
mapFst f (a, b) = (f a, b)
mapSnd f (a, b) = (a, f b)

-- * Config type

data SimConfig = SimConfig {
                           } deriving (Show)

-- * General types

type Health = Integer
type Damage = Integer

-- ** IDs
newtype EntityId = EntityId String deriving (Eq, Ord)
instance Show EntityId where
    show (EntityId name) = "Entity." ++ name

newtype AbilityId = AbilityId String deriving (Eq, Ord)
instance Show AbilityId where
    show (AbilityId name) = "Ability." ++ name

newtype AuraId = AuraId String deriving (Eq, Ord)
instance Show AuraId where
    show (AuraId name) = "Aura." ++ name

-- ** Monads
data ActionState = ActionState { _actionSource :: EntityId
                               , _actionTarget :: EntityId
                               } deriving (Show)

type Action = ReaderT ActionState (Sim World Event)

-- ** Handlers

type Handler = Event -> Action ()
type HandlerId = String
type HandlerList = Map HandlerId Handler

-- ** Buffs

type Buff     = Stats -> Stats
type BuffId   = String
type BuffList = Map BuffId Buff

-- ** Entity
type EntityMap = Map EntityId Entity

data Entity = Entity { _eID             :: !EntityId
                     , _eTarget         :: !EntityId
                     , _eHealth         :: !Health
                     , _eGlobalCD       :: !Time
                     , _eCast           :: Maybe (Ability, Time)
                     , _eCooldowns      :: Map AbilityId Time
                     , _eStats          :: Stats
                     , _eBaseStats      :: Stats
                     , _eAuras          :: AuraList
                     , _eAI             :: Handler
                     , _eHandlers       :: HandlerList
                     , _eAttFlatBuffs   :: BuffList
                     , _eAttMultBuffs   :: BuffList
                     , _ePriFlatBuffs   :: BuffList
                     , _ePriMultBuffs   :: BuffList
                     , _eSecFlatBuffs   :: BuffList
                     , _eSecMultBuffs   :: BuffList
                     }

instance Show Entity where
    show (Entity id targ health gcd cast cds stats bStats auras ai hs buffs _ _ _ _ _) =
        printf "Entity { eId = %s, eTarget = %s, eHealth = %d, eGlobalCD = %f, eCast = %f, eCooldowns = %s, eStats = %s, eAuras = %s, eHandlers = %s, eBuffs = %s }"
            (show id)
            (show targ)
            health
            gcd
            (fromMaybe 0.0 (snd `fmap` cast))
            (show cds)
            (show stats)
            (show . elems $ auras)
            (show . keys $ hs)
            (show . keys $ buffs)



data World = World { _wEntities :: EntityMap
                   , _wGen      :: !StdGen
                   } deriving (Show)

data Event = EvSimStart
           | EvMessage String
           | EvAction (Sim World Event ())
           | EvGcdEnd EntityId
           | EvCooldownExpire EntityId AbilityId
           -- Autoattack start/stop ready
           | EvAutoAttackStart EntityId
           | EvAutoAttackStop  EntityId
           | EvAutoAttackReady EntityId
           | EvCastStarted     EntityId AbilityId
           | EvCastComplete    EntityId AbilityId
           | EvCastInterrupted EntityId AbilityId
           | EvHit   EntityId EntityId AbilityId Damage
           | EvCrit  EntityId EntityId AbilityId Damage
           | EvDodge EntityId EntityId AbilityId
           | EvParry EntityId EntityId AbilityId
           | EvMiss  EntityId EntityId AbilityId
           | EvBuffsChanged     EntityId
           | EvAuraApplied   EntityId EntityId AuraId
           | EvAuraExpire    EntityId EntityId AuraId
           --deriving (Show)

type AbilityMap = Map AbilityId Ability
data Ability = Ability { _abilName       :: AbilityId
                       , _abilCooldown   :: Maybe DTime
                       , _abilTriggerGCD :: Bool
                       , _abilCastTime   :: DTime
                       , _abilSchool     :: SpellSchool
                       , _abilAction     :: Action ()
                       }
instance Show Ability where
    show (Ability name _ _ _ _ _) = "Ability." ++  (show name)

-- * Auras
-- | Auras come in two flavors, those that buff, and those that don't.  The
-- buffs have a type, which is a category of stats they can affect, a buff
-- function that is used to re-evaluate the stats, and an Id.  Other Auras just
-- have an ID to identify themselves and are just there to check whether a buff
-- exists.

data AuraType   = BeneficialAura
                | DebuffMagic
                | DebuffCurse
                | DebuffPoison
                | DebuffDisease
                | DebuffBleed
                | DebuffOther
                deriving (Show, Eq, Typeable, Data)

type AuraList = Map AuraId Aura
data Aura = Aura { _auraId          :: AuraId
                 , _auraOwner       :: EntityId
                 , _auraSchool      :: SpellSchool
                 , _auraType        :: AuraType
                 , _auraDuration    :: DTime
                 } deriving (Show)

{-
instance Show Aura where
    show (Aura name owner school type duration _ _) = printf "Aura.%s.%s" (show owner) (show name)
    -}


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

data AttackResult = ResultHit Damage
                  | ResultCrit Damage
                  | ResultDodge
                  | ResultParry
                  | ResultMiss
                    deriving (Eq, Show, Typeable, Data)

-- * Stats

type StatMod = Stats -> Stats

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

$(mkLabels [''Ability, ''Action, ''ActionState, ''Entity, ''World, ''Stats, ''Aura])
