module AI.AutoAttack where

import DisEvSim
import Types.Events
import Types.World
import AI.Info

import Actions.Attacks

autoAttack :: Event -> Sim World Event ()
autoAttack (EvGcdEnd _) =
    do  ingcd <- playerOnGCD
        if (ingcd)
            then return ()
            else attack 100
autoAttack _ = return ()
