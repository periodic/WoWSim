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
    let target' = target { health = health target + dmg }
    putW (World player target')
    resetGCD
