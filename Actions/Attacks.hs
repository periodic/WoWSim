module Actions.Attacks where

import DisEvSim
import Types.World
import Types.Events

import Actions.Common

attack :: Damage -> Sim World Event ()
attack dmg = do
    (World player target) <- getW
    t                     <- getT
    after 0 (EvSwingDamage (eID player) (eID target) dmg)
    let target' = target { eHealth = eHealth target + dmg }
    putW (World player target')

useAbility :: Ability -> Sim World Event ()
useAbility abil = do
    (World player target) <- getW
    t                     <- getT
    if (abilTriggerGCD abil)
        then resetGCD
        else return()
    case (abilCooldown abil) of
        Nothing -> return()
        Just dt -> do
            setCooldown (abilName abil) (t + dt)
            after dt EvCooldownExpire
    abilAction abil
