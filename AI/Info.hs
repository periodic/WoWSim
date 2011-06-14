module AI.Info where

import Types.World
import Actions.Common

playerOnGCD :: Action Bool
playerOnGCD =
    do  (World player _) <- getW
        t                <- getT
        return $ onGCD player t

playerAbilOnCooldown :: String -> Action Bool
playerAbilOnCooldown name =
    do  (World player _) <- getW
        t                <- getT
        return $ onCooldown player name t
