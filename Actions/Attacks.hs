module Actions.Attacks where

import Types.World

import Actions.Common

attack :: AbilityId -> Damage -> Action ()
attack abilName dmg = do
    (World player target) <- getW
    t                     <- getT
    after 0 (EvSwingDamage (eID player) (eID target) abilName dmg)
    let target' = target { eHealth = eHealth target + dmg }
    putW (World player target')


startAutoAttack :: EntityId -> DTime -> Damage -> Action ()
startAutoAttack owner timer dmg = do
    addHandler name autoAttackHandler
    after 0 (EvAutoAttackReady owner)
    where
        name = "AutoAttack"
        autoAttackHandler (EvAutoAttackReady eid) 
            | eid == owner = do 
                attack name dmg
                after timer (EvAutoAttackReady owner)
            | otherwise = return ()
        autoAttackHandler _ = return ()
