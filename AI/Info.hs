module AI.Info where

import DisEvSim
import Types.Events
import Types.World

playerOnGCD :: Sim World Event Bool
playerOnGCD =
    do  (World player _) <- getW
        t                <- getT
        return $ onGCD player t
