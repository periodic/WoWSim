module AI.AutoAttack where

import DisEvSim
import Types.World
import AI.Info

import Actions.Common
import Actions.Attacks

autoAttack :: Event -> Action ()
autoAttack (EvSimStart) = swing
autoAttack (EvGcdEnd _) =
    do  ingcd <- onGCD
        if (ingcd)
            then return ()
            else swing
autoAttack _ = return ()

swing = do
    resetGCD
    attack (AbilityId "AutoAttack") 100
