module Actions.Common where

import DisEvSim
import Types.World
import Types.Events

resetGCD :: Sim World Event ()
resetGCD =
    do  w@(World player _)  <- getW
        t                   <- getT
        let player' = player { globalCD = t + 1.5 }
        putW $ w { player = player' }
        after 1.5 . EvGcdEnd . eID $ player
