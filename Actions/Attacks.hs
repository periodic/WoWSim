module Actions.Attacks where

import DisEvSim
import Types.World

import Actions.Common

attack :: Damage -> Sim World Event ()
attack dmg = do
    (World player target) <- getW
    t                     <- getT
    after 0 (EvSwingDamage (eID player) (eID target) dmg)
    let target' = target { eHealth = eHealth target + dmg }
    putW (World player target')


autoAttack :: Damage -> Sim World Event ()
autoAttack = undefined -- TODO: waiting for addHandler
