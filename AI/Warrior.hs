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
    oncd  <- abilOnCooldown msName
    if ingcd || casting
        then return()
        else do
            hasRend <- targetHasAura rendDebuff
            if not hasRend
                then useAbility rend
                else if oncd
                    then useAbility slam
                    else useAbility mortalStrike
    where
        msName = AbilityId "MortalStrike"
        mortalStrike =
            Ability { _abilName       = msName
                    , _abilCooldown   = Just 6
                    , _abilTriggerGCD = True
                    , _abilCastTime   = 0
                    , _abilSchool     = Physical
                    , _abilAction     = weapon msName 2 100
                    }
        slamName = AbilityId "Slam"
        slam = 
            Ability { _abilName       = slamName
                    , _abilCooldown   = Nothing
                    , _abilTriggerGCD = True
                    , _abilCastTime   = 2.0
                    , _abilSchool     = Physical
                    , _abilAction     = weapon slamName 1 100
                    }
        rendName = AbilityId "Rend"
        rendDebuff = AuraId "Rend"
        rend = 
            Ability { _abilName       = rendName
                    , _abilCooldown   = Nothing
                    , _abilTriggerGCD = True
                    , _abilCastTime   = 0
                    , _abilSchool     = Physical
                    , _abilAction     = do
                        addTargetAura rendAura
                        doIn 3 rendHandler
                    }
        rendHandler :: Action ()
        rendHandler = do
            hasRend <- targetHasAura rendDebuff
            if hasRend
                then attack rendName 100 >> doIn 3 rendHandler
                else doNothing
        -- Rend needs to:
        --   Remove it's handler on expiry?
        --   Be able to trigger events at intervals that other handlers don't need to see.
        rendAura = Aura { _auraId          = rendDebuff
                        , _auraOwner       = EntityId "SET ME!"
                        , _auraSchool      = Physical
                        , _auraType        = DebuffBleed
                        , _auraDuration    = 15
                        }

defaultWarriorTalents :: TalentList
defaultWarriorTalents = [(TalentId "Test Talent", 3)]

warriorTalentMap :: TalentMap
warriorTalentMap = fromList [(TalentId "Test Talent", Talent (TalentId "Test Talent") (const $ modL (strength <.> eBaseStats) (+100)))]
