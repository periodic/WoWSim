module AI.Warrior where

import DisEvSim
import Types.World
import AI.Info

import Actions.Common
import Actions.Attacks

warrior :: Event -> Sim World Event ()
warrior (EvSimStart)        = rotation
warrior (EvGcdEnd _)        = rotation
warrior (EvCooldownExpire)  = rotation
warrior _                   = return ()

rotation = do
    ingcd <- playerOnGCD
    oncd  <- playerAbilOnCooldown "Mortal Strike"
    if (ingcd || oncd)
        then return ()
        else useAbility mortalStrike
    where
        mortalStrike =
            Ability { abilName       = "Mortal Strike"
                    , abilCooldown   = Just 6
                    , abilTriggerGCD = True
                    , abilAction     = attack 100
                    }
