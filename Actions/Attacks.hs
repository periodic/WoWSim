module Actions.Attacks where

import Types.World
import Types.Stats

import Actions.Common

attack :: AbilityId -> Damage -> Action ()
attack abilName dmg = do
    --src <- getSource
    --trg <- getTarget
    --after 0 (EvSwingDamage (getL eID src) (getL eID trg) abilName dmg)
    logAttack abilName $ ResultHit dmg

-- |Make an attack with the equipped weapon, with a multiplicative modifier and flat bonus.
weapon :: AbilityId -> Float -> Damage -> Action ()
weapon abilName mult bonus = do
    src <- getSource
    trg <- getTarget
    result <- withRandom $ weaponAttack (getL eStats src) (getL eStats trg) mult bonus
    logAttack abilName result

logAttack :: AbilityId -> AttackResult -> Action ()
logAttack name res = do
    sid <- getL eID <$> getSource
    tid <- getL eID <$> getTarget
    case res of
        ResultHit dmg   -> modifyTarget (modL eHealth (+ dmg)) >> (after 0 $ EvHit   sid tid name dmg)
        ResultCrit dmg  -> modifyTarget (modL eHealth (+ dmg)) >> (after 0 $ EvCrit  sid tid name dmg)
        ResultParry     -> after 0 $ EvParry sid tid name
        ResultDodge     -> after 0 $ EvDodge sid tid name
        ResultMiss      -> after 0 $ EvMiss  sid tid name
    

startAutoAttack :: Action ()
startAutoAttack = do
    src <- getSource
    let owner = getL eID src
    addHandler name (autoAttackHandler owner)
    after 0 (EvAutoAttackReady owner)
    where
        name = "AutoAttack"
        autoAttackHandler owner (EvAutoAttackReady eid) 
            | eid == owner = do 
                delay <- getL (weaponSpeed <.> eStats) <$> getSource
                weapon name 1.0 0
                after delay (EvAutoAttackReady owner)
            | otherwise = return ()
        autoAttackHandler _ _ = return ()
