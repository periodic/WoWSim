module AI.Warrior where

import Types.World
import AI.Info

import Actions.Common
import Actions.Attacks

warrior :: Event -> Action ()
warrior (EvSimStart)            = startAutoAttack >> rotation
warrior (EvGcdEnd _)            = rotation
warrior (EvCooldownExpire _ _)  = rotation
warrior _                       = return ()

rotation = do
    ingcd <- onGCD
    oncd  <- abilOnCooldown msName
    if ingcd
        then return()
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
                    , _abilCastTime   = 1.5
                    , _abilSchool     = Physical
                    , _abilAction     = weapon slamName 1 100
                    }
        rendName = AbilityId "Rend"
        rend = 
            Ability { _abilName       = rendName
                    , _abilCooldown   = Nothing
                    , _abilTriggerGCD = True
                    , _abilCastTime   = 0
                    , _abilSchool     = Physical
                    , _abilAction     = do
                        addTargetAura rendAura
                        addHandler "Rend" rendHandler
                    }
        rendHandler :: Event -> Action ()
        rendHandler (EvAuraExpire _ _ (AuraId "Rend")) = removeHandler "Rend"
        -- Rend needs to:
        --   Remove it's handler on expiry?
        --   Be able to trigger events at intervals that other handlers don't need to see.
        rendAura = Aura { _auraId          = AuraId "Rend"
                        , _auraOwner       = EntityId "SET ME!"
                        , _auraSchool      = Physical
                        , _auraType        = DebuffBleed
                        , _auraDuration    = 15
                        }
