module AI.Warrior where

import Types.World
import AI.Info

import Actions.Common
import Actions.Attacks

import Control.Monad.Reader
import Data.Map (empty, fromList)

warrior :: Event -> Action ()
warrior (EvSimStart)            = startAutoAttack >> rotation
warrior (EvGcdEnd _)            = rotation
warrior (EvCooldownExpire _ _)  = rotation
warrior (EvCastComplete   _ _)  = rotation
warrior _                       = return ()

rotation = do
    ingcd <- onGCD
    casting <- iAmCasting
    oncd  <- abilOnCooldown mortalStrike
    if ingcd || casting
        then return()
        else do
            hasRend <- targetHasAura rendDebuff
            if not hasRend
                then useAbility rend
                else if oncd
                    then useAbility slam
                    else useAbility mortalStrike


mortalStrike = AbilityId "MortalStrike"
mortalStrikeAbil =
    Ability { _abilName       = mortalStrike
            , _abilCooldown   = Just 6
            , _abilTriggerGCD = True
            , _abilCastTime   = 0
            , _abilSchool     = Physical
            , _abilAction     = weapon mortalStrike 2 100
            }

slam = AbilityId "Slam"
slamAbil =
    Ability { _abilName       = slam
            , _abilCooldown   = Nothing
            , _abilTriggerGCD = True
            , _abilCastTime   = 2.0
            , _abilSchool     = Physical
            , _abilAction     = weapon slam 1 100
            }

rend = AbilityId "Rend"
rendDebuff = AuraId "Rend"
rendAbil =
    Ability { _abilName       = rend
            , _abilCooldown   = Nothing
            , _abilTriggerGCD = True
            , _abilCastTime   = 0
            , _abilSchool     = Physical
            , _abilAction     = do
                addTargetAura rendAura
                doIn 3 rendHandler
            }
    where
        rendHandler = do
            hasRend <- targetHasAura rendDebuff
            if hasRend
                then attack rend 100 >> doIn 3 rendHandler
                else doNothing
        rendAura = Aura { _auraId          = rendDebuff
                        , _auraOwner       = EntityId "SET ME!"
                        , _auraSchool      = Physical
                        , _auraType        = DebuffBleed
                        , _auraDuration    = 15
                        }

warriorAbilities :: AbilityMap
warriorAbilities = makeAbilityMap [mortalStrikeAbil, slamAbil, rendAbil]
    where
        makeAbilityMap = foldr addAbilityToMap emptyAbilityMap

defaultWarriorTalents :: TalentList
defaultWarriorTalents = [(TalentId "Test Talent", 3)]

warriorTalentMap :: TalentMap
warriorTalentMap = fromList [(TalentId "Test Talent", Talent (TalentId "Test Talent") (const $ modL (strength <.> eBaseStats) (+100)))]
