module AI.Warrior where

import DisEvSim
import Types.World
import AI.Info

import Actions.Common
import Actions.Attacks

warrior :: Event -> Action ()
warrior (EvSimStart)            = startAutoAttack 2.0 100 >> rotation
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
            Ability { abilName       = msName
                    , abilCooldown   = Just 6
                    , abilTriggerGCD = True
                    , abilAction     = attack msName 100
                    }
