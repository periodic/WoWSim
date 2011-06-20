module Actions.Attacks where

import Types.World

import Actions.Common

attack :: AbilityId -> Damage -> Action ()
attack abilName dmg = do
    src <- getSource
    trg <- getTarget
    modifyTarget $ modL eHealth (+ dmg)
    after 0 (EvSwingDamage (getL eID src) (getL eID trg) abilName dmg)

-- |Make an attack with the equipped weapon, with a multiplicative modifier and flat bonus.
--weapon :: AbilityId -> Float -> Damage -> Action ()
--weapon abilName mult bonus = do
    

startAutoAttack :: DTime -> Damage -> Action ()
startAutoAttack timer dmg = do
    src <- getSource
    let owner = getL eID src
    addHandler name (autoAttackHandler owner)
    after 0 (EvAutoAttackReady owner)
    where
        name = "AutoAttack"
        autoAttackHandler owner (EvAutoAttackReady eid) 
            | eid == owner = do 
                attack name dmg
                after timer (EvAutoAttackReady owner)
            | otherwise = return ()
        autoAttackHandler _ _ = return ()
