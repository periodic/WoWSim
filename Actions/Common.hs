module Actions.Common where

import DisEvSim
import Types.World
import Types.Events

resetGCD :: Sim World Event ()
resetGCD =
    do  w@(World player _)  <- getW
        t                   <- getT
        let player' = player { eGlobalCD = t + 1.5 }
        putW $ w { player = player' }
        after 1.5 . EvGcdEnd . eID $ player

setCooldown :: String -> DTime -> Sim World Event ()
setCooldown name dt =
    do  w@(World player _) <- getW
        t                  <- getT
        let player' = addCooldown player name (t + dt)
        putW $ w { player = player' }


