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
    show (EvHit pid tid abil dmg)       = printf "%s's %s hit %s for %d" (show pid) abil (show tid) dmg
    show (EvCrit pid tid abil dmg)      = printf "%s's %s crit %s for %d" (show pid) abil (show tid) dmg
    show (EvDodge pid tid abil)         = printf "%s dodged %s's %s" (show tid) (show pid) abil
    show (EvParry pid tid abil)         = printf "%s parried %s's %s" (show tid) (show pid) abil
    show (EvMiss pid tid abil)          = printf "%s %s missed %s" (show pid) abil (show tid)
