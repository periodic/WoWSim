module AI.Info where

import DisEvSim
import Types.World

playerOnGCD :: Sim World Event Bool
playerOnGCD =
    do  (World player _) <- getW
        t                <- getT
        return $ onGCD player t

playerAbilOnCooldown :: String -> Sim World Event Bool
playerAbilOnCooldown name =
    do  (World player _) <- getW
        t                <- getT
        return $ onCooldown player name t
