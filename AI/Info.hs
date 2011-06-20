module AI.Info where

import Types.World
import Actions.Common

onGCD :: Action Bool
onGCD = do
    s <- getSource
    t <- getTime
    return $ entityOnGCD t s

abilOnCooldown :: String -> Action Bool
abilOnCooldown name = do
    s <- getSource
    t <- getTime
    return $ entityOnCooldown name t s
