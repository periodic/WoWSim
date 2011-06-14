module Types.Event where

import Types.Common

import Text.Printf

instance Show Event where
    show EvSimStart                     = "Sim Start"
    show (EvGcdEnd eid)                 = printf "GCD end for %s" (show eid)
    show (EvCooldownExpire eid name)    = printf "Cooldown Expired for %s's %s" (show eid) name
    show (EvAutoAttackStart eid)        = printf "Autoattack started for %s" (show eid)
    show (EvAutoAttackStop  eid)        = printf "Autoattack stopped for %s" (show eid)
    show (EvAutoAttackReady eid)        = printf "Autoattack ready for %s" (show eid)
    show (EvSwingDamage pid tid abil dmg) = printf "%s's %s hit %s for %d" (show pid) abil (show tid) dmg
