module Types.Event where

import Types.Common

instance Show Event where
    show EvSimStart                     = "Sim Start"
    show (EvGcdEnd eid)                 = "GCD end for " ++ show eid
    show EvCooldownExpire               = "Cooldown Expired"
    show EvAutoAttackReady              = "Autoattack Ready"
    show (EvSwingDamage pid tid dmg)    = "Swing: " ++ show pid ++ " hit " ++ show tid ++ " for " ++ show dmg
