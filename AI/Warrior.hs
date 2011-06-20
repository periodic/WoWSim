module AI.Warrior where

import DisEvSim
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
    if (ingcd || oncd)
        then return ()
        else useAbility mortalStrike
    where
        msName = "MortalStrike"
        mortalStrike =
            Ability { _abilName       = msName
                    , _abilCooldown   = Just 6
                    , _abilTriggerGCD = True
                    , _abilAction     = weapon msName 1 100
                    }
