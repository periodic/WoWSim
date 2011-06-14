module AI.AutoAttack where

import DisEvSim
import Types.World
import AI.Info

import Actions.Common
import Actions.Attacks

autoAttack :: Event -> Sim World Event ()
autoAttack (EvSimStart) = swing
autoAttack (EvGcdEnd _) =
    do  ingcd <- playerOnGCD
        if (ingcd)
            then return ()
            else swing
autoAttack _ = return ()

swing = do
    resetGCD
    attack "AutoAttack" 100
