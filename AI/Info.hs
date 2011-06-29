module AI.Info where

import Types.World
import Actions.Common

onGCD :: Action Bool
onGCD = do
    s <- getSource
    t <- getTime
    return $ entityOnGCD t s

abilOnCooldown :: AbilityId -> Action Bool
abilOnCooldown name = do
    s <- getSource
    t <- getTime
    return $ entityOnCooldown name t s

iHaveAura :: AuraId -> Action Bool
iHaveAura name = do
    s <- getSource
    return . isAuraInList name . getL eAuras $ s

targetHasAura :: AuraId -> Action Bool
targetHasAura name = do
    t <- getTarget
    return . isAuraInList name . getL eAuras $ t

iAmCasting :: Action Bool
iAmCasting = do
    s <- getSource
    t <- getTime
    case (getL eCast s) of
        Just (_, ct) -> return (ct > t)
        Nothing      -> return False
