module AI.Warrior where

import DisEvSim
import Types.World
import AI.Info

import Actions.Common
import Actions.Attacks

warrior :: EntityId -> Event -> Sim World Event ()
warrior eid (EvSimStart)            = startAutoAttack eid 2.0 100 >> rotation eid
warrior eid (EvGcdEnd _)            = rotation eid
warrior eid (EvCooldownExpire _ _)  = rotation eid
warrior eid _                       = return ()

rotation eid = do
    ingcd <- playerOnGCD
    oncd  <- playerAbilOnCooldown msName
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
