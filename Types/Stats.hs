{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Types.Stats ( Stats (..)
                   , Class (..)
                   , PowerType (..)
                   , AttackResult (..)
                   , AuraType (..)
                   , RuneType
                   , HitFunc
                   , SpellSchool (..)
                   , physHitFunc
                   , spellHitFunc
                   , defaultStats
                   , actualSpellDamage
                   , actualSpellDamageCoeff
                   , actualPhysDamage
                   , actualSpellCastTime
                   , actualMeleeCastTime
                   , weaponAttack
                   {-
                   , addAttributeBonus
                   , addAttributeMult
                   , addPrimaryBonus
                   , addPrimaryMult
                   , addSecondaryBonus
                   , addSecondaryMult
                   , addSpellPowerBonus
                   , addSpellPowerMult
                   , statModifiers
                   , applyStatModifiers
                   -}
                   ) where

import Prelude hiding (gcd)

import System.Random
import Data.Data
import Data.Typeable
import Data.Map hiding (map)
import Data.Record.Label

import DisEvSim (DTime, Time, Sim)
import Types.Common

baseSpellMiss alevel tlevel =
    case tlevel - alevel of
        5 -> 0.39
        4 -> 0.28
        3 -> 0.17
        2 -> 0.06
        1 -> 0.05
        0 -> 0.04
        -1 -> 0.03
        -2 -> 0.02
        -3 -> 0.01
        _  -> 0.00

basePhysMiss alevel tlevel = 0.08 -- TODO: get a real table.

dodgeRate c = 
    case c of
        Warrior     -> 84.74576271 
        Paladin     -> 59.88023952
        DeathKnight -> 84.74576271 
        Hunter      -> 86.20689655 
        Shaman      -> 59.88023952 
        Druid       -> 47.84688995 
        Rogue       -> 47.84688995 
        Mage        -> 58.82352941 
        Priest      -> 59.88023952 
        Warlock     -> 59.88023952 

baseDodge c =
    case c of
        Warrior      ->  3.66400
        Paladin      ->  3.49430
        Hunter       -> -4.08730
        Rogue        ->  2.09570
        Priest       ->  3.41780
        DeathKnight  ->  3.66400
        Shaman       ->  2.10800
        Mage         ->  3.65870
        Warlock      ->  2.42110
        Druid        ->  5.60970 

physHitFunc :: HitFunc
physHitFunc aStats tStats hitDmg critDmg gen = 
    let (r, gen') = random gen
    in miss r gen'
    where
        missChance   = baseSpellMiss (getL level aStats)
                                     (getL level tStats)
                       - getL meleeHit aStats
        miss     r g = if r < missChance
                       then (ResultMiss, g)
                       else dodge (r - missChance) g
        dodge    r g = if r < getL dodgeChance tStats
                       then (ResultDodge, g)
                       else parry (r - getL dodgeChance tStats) g
        parry    r g = if r < getL parryChance tStats
                       then (ResultParry, g)
                       else glancing (r - getL parryChance tStats) g
        glancing r g = crit r g
        crit     r g = if r < getL meleeCrit aStats
                       then (ResultCrit critDmg, g)
                       else (ResultHit hitDmg, g)

spellHitFunc :: HitFunc
spellHitFunc aStats tStats hitDmg critDmg gen =
    let (r, gen') = random gen
     in miss r gen'
     where 
        missChance = max 0
                         (baseSpellMiss (getL level aStats) 
                                        (getL level tStats) 
                         - getL spellHit aStats) 
        miss r g = if   r < missChance
                   then (ResultMiss, g)
                   else crit (r - missChance) g
        crit r g = if   r < (getL spellCrit aStats)
                   then (ResultCrit critDmg, g)
                   else (ResultHit hitDmg,  g)

weaponDamage :: Stats -> Stats -> StdGen -> (Damage, StdGen)
weaponDamage aStats tStats gen =
    let (wdmg, gen') = randomR (minDmg aStats, maxDmg aStats) gen
        dmg          = truncate $ (fromIntegral wdmg) 
                                * (getL physMult aStats) 
     in (actualPhysDamage aStats tStats dmg, gen')
    where
        ap2dps stats = fromIntegral (getL attackPower stats) * (getL weaponSpeed stats) / 14
        --avgDmg stats = (minDmg stats + maxDmg stats) `div` 2
        minDmg stats = (getL weaponMinDamage stats) 
                        + truncate (ap2dps stats * getL weaponSpeed stats)
        maxDmg stats = (getL weaponMaxDamage stats) 
                        + truncate (ap2dps stats * getL weaponSpeed stats)

weaponAttack :: Stats -> Stats -> StdGen -> (AttackResult, StdGen)
weaponAttack aStats tStats gen = 
    let (dmgGen, hitGen)    = System.Random.split gen
        (dmg, _)            = weaponDamage aStats tStats dmgGen
        critDmg             = truncate . (* getL physCritMult aStats) . fromIntegral $ dmg
     in physHitFunc aStats tStats dmg critDmg hitGen

actualPhysDamage :: Stats -> Stats -> Health -> Health
actualPhysDamage aStats tStats dmg = truncate $ (1 - reduction) * (fromIntegral dmg)
    where
        reduction    = armorReduction (getL level aStats) (truncate $ (1 - arpReduction) * (fromIntegral . getL armor $ tStats))
        arpReduction = (fromIntegral . getL armorPen $ aStats) / 1399 -- 80 only

armorReduction :: Integer -> Integer -> Float
armorReduction level armor = if level >= 60
                             then over60
                             else under60
    where
        fArmor  = fromIntegral armor
        fLevel  = fromIntegral level
        under60 = fArmor / (fArmor + 400 + 85 * fLevel)
        over60  = fArmor / (fArmor + (400 + 85 * (fLevel + 4.5 * (fLevel - 50))))

{- Given the spell school, get the right stats selector -}
getSpellPower school =
    case school of
        Arcane  -> getL arcaneSpellDamage
        Fire    -> getL fireSpellDamage
        Frost   -> getL frostSpellDamage
        Nature  -> getL natureSpellDamage
        Holy    -> getL holySpellDamage
        Shadow  -> getL shadowSpellDamage

getSpellMult school =
    case school of
        Arcane  -> getL arcaneDamageMult
        Fire    -> getL fireDamageMult
        Frost   -> getL frostDamageMult
        Nature  -> getL natureDamageMult
        Holy    -> getL holyDamageMult
        Shadow  -> getL shadowDamageMult

{- Returns the actual spell damage given the base damage, casting time, spell
 - school, and the player stats. -}
actualSpellDamage :: DTime -> SpellSchool -> Stats -> Health -> Health
actualSpellDamage baseCastTime school stats dmg 
    = actualSpellDamageCoeff 1.0 1.0 baseCastTime school stats dmg

actualSpellDamageCoeff :: Float -> Float -> DTime -> SpellSchool -> Stats -> Health -> Health
actualSpellDamageCoeff coeff spCoeff baseCastTime school stats dmg 
    = let ctmod = realToFrac $ max (baseCastTime / 3.5) (0.4286)
          mult  = getSpellMult school stats * getL spellDamageMult stats
          sp    = truncate . (* ctmod) . (* coeff) . (* spCoeff) . fromIntegral . getSpellPower school $ stats
       in truncate . (* mult) . fromIntegral $ dmg + sp

{- Haste calculations -}

spellHasteMultiplier :: (Fractional a) => Stats -> a
spellHasteMultiplier stats = fromRational . toRational $ 1 - getL spellHaste stats

meleeHasteMultiplier :: (Fractional a) => Stats -> a
meleeHasteMultiplier stats = fromRational . toRational $ 1 - getL meleeHaste stats

actualSpellCastTime :: DTime -> Stats -> DTime
actualSpellCastTime dt stats = dt * spellHasteMultiplier stats

actualMeleeCastTime :: DTime -> Stats -> DTime
actualMeleeCastTime dt stats = dt * meleeHasteMultiplier stats

{- Modifier functions -}

{- Default attribute bonuses are bonsues and multipliers to the base attributes. These are counted first. 
 - Examples:
 - 10% bonus to all stats
 - +200 stamina
 - -}
{-
defaultAttributeAdditive :: StatMod
defaultAttributeAdditive stats = stats
addAttributeBonus :: StatMod -> Stats -> Stats
addAttributeBonus mod = modL attributeBonus (. mod)

defaultAttributeMult :: StatMod
defaultAttributeMult stats = stats
addAttributeMult :: StatMod -> Stats -> Stats
addAttributeMult mod = modL attributeMult (. mod)
-}

{- Primary stats are generally rating. 
 - Examples:
 - +200 crit rating
 - Strength -> Parry Rating
 - -}
{-
defaultPrimaryAdditive :: StatMod
defaultPrimaryAdditive stats = stats
addPrimaryBonus :: StatMod -> Stats -> Stats
addPrimaryBonus mod = modL primaryStatBonus (. mod)

defaultPrimaryMult :: StatMod
defaultPrimaryMult stats = stats
addPrimaryMult :: StatMod -> Stats -> Stats
addPrimaryMult mod = modL primaryStatMult (. mod)

{- Secondary mods are all the actual combat stats. 
 - Examples:
 - Crit Rating -> Crit %
 - Stamina -> Health
 - -}
defaultSecondaryAdditive :: StatMod
defaultSecondaryAdditive stats =
    (modL attackPower (+ case (getL characterClass stats) of
                                Warrior     -> 2 * (getL strength stats)
                                DeathKnight -> 2 * (getL strength stats)
                                Druid       -> 2 * (getL strength stats)
                                Paladin     -> 2 * (getL strength stats)
                                Rogue       -> (getL strength stats) + (getL agility stats)
                                Hunter      -> (getL strength stats) + (getL agility stats)
                                Shaman      -> (getL strength stats) + (getL agility stats)
                                Mage        -> (getL strength stats)
                                Warlock     -> (getL strength stats)
                                Priest      -> (getL strength stats)
                      )) . 
    (foldr (\sp f -> f . modL sp (+ getL intellect stats)) id [ arcaneSpellDamage
                                                              , fireSpellDamage
                                                              , frostSpellDamage
                                                              , holySpellDamage
                                                              , natureSpellDamage
                                                              , shadowSpellDamage
                                                              ]) .
    (modL spellHaste (+ (fromIntegral . getL hasteRating   $ stats) / 3279)) .
    (modL spellHit   (+ (fromIntegral . getL hitRating     $ stats) / 2623)) .
    (modL spellCrit  ((+ (fromIntegral . getL intellect     $ stats) / 16666.67) .
                      (+ (fromIntegral . getL critRating    $ stats) / 4591))) .
    (modL meleeHaste (+ (fromIntegral . getL hasteRating   $   stats) / 3279)) .
    (modL meleeHit   (+ (fromIntegral . getL hitRating     $ stats) / 3074)) .
    (modL meleeCrit  (+ (fromIntegral . getL critRating    $ stats) / 4591)) .
    (modL mastery    (+ (fromIntegral . getL masteryRating $ stats) / 4591)) .
    (modL powerMax   (+ if (getL powerType stats == Mana)
                            then 15 * (getL intellect stats - 20) + 20
                            else 0)
                     ) . 
    (modL healthMax  (+ (10 * (getL stamina stats - 20) + 20)))
    $ stats
addSecondaryBonus :: StatMod -> Stats -> Stats
addSecondaryBonus mod = modL secondaryStatBonus (. mod)

defaultSecondaryMult :: StatMod
defaultSecondaryMult stats = stats
addSecondaryMult :: StatMod -> Stats -> Stats
addSecondaryMult mod = modL secondaryStatMult (. mod)

addSpellPowerBonus :: Integer -> StatMod
addSpellPowerBonus i = 
    let m = modL arcaneSpellDamage (+ i)
          . modL fireSpellDamage   (+ i)
          . modL frostSpellDamage  (+ i)
          . modL holySpellDamage   (+ i)
          . modL natureSpellDamage (+ i)
          . modL shadowSpellDamage (+ i)
     in addSecondaryBonus m

addSpellPowerMult :: Float -> StatMod
addSpellPowerMult mult =
    let f = truncate . (* mult) . fromIntegral 
        m = modL arcaneSpellDamage f
          . modL fireSpellDamage   f
          . modL frostSpellDamage  f
          . modL holySpellDamage   f
          . modL natureSpellDamage f
          . modL shadowSpellDamage f
     in addSecondaryMult m
-}

{- Default stat stuff -}

defaultStats = Stats { _level              = 80
                     , _characterClass     = Warrior
                     -- Ability Scores
                     , _strength           = 0
                     , _agility            = 0
                     , _stamina            = 0
                     , _intellect          = 0
                     , _spirit             = 0
                     -- ratings
                     , _critRating         = 0
                     , _spellCrit          = 0
                     , _meleeCrit          = 0
                     , _hasteRating        = 0
                     , _meleeHaste         = 0
                     , _spellHaste         = 0
                     , _hitRating          = 0
                     , _spellHit           = 0
                     , _meleeHit           = 0
                     , _masteryRating      = 0
                     , _mastery            = 0
                     -- Melee stats
                     , _attackPower        = 0
                     , _armorPen           = 0
                     , _armorIgnored       = 0
                     -- Weapon stats
                     , _weaponSpeed         = 2.0
                     , _weaponMinDamage     = 100
                     , _weaponMaxDamage     = 200 
                     -- Magic stats
                     , _arcaneSpellDamage  = 0
                     , _fireSpellDamage    = 0
                     , _frostSpellDamage   = 0
                     , _holySpellDamage    = 0
                     , _natureSpellDamage  = 0
                     , _shadowSpellDamage  = 0

                     , _arcaneDamageMult   = 1.0
                     , _fireDamageMult     = 1.0
                     , _frostDamageMult    = 1.0
                     , _holyDamageMult     = 1.0
                     , _natureDamageMult   = 1.0
                     , _shadowDamageMult   = 1.0

                     , _spellDamageMult    = 1.0

                     , _spellCostMultOff   = 1.0
                     , _spellCostMultDef   = 1.0
                     -- Defenses
                     , _armor              = 0
                     , _damageReduction    = 0
                     , _parryRating        = 0
                     , _parryChance        = 0
                     , _dodgeRating        = 0
                     , _dodgeChance        = 0
                     , _defenseRating      = 0
                     , _missChance         = 0
                     , _critReduction      = 0
                     -- Power Source
                     , _powerType          = Mana
                     , _powerMax           = 0
                     , _powerRegen         = 0
                     -- Health
                     , _healthMax          = 0
                     -- Other Status
                     , _gcd                 = 1.5
                     , _physCritMult        = 2.0
                     , _physMult            = 1.0
                     , _spellCritMult       = 1.5
                     , _spellMult           = 1.0
                     {-
                     , _spellBook          = emptySpellBook
                     -- Stat modifiers
                     , _attributeBonus     = defaultAttributeAdditive
                     , _attributeMult      = defaultAttributeMult
                     , _primaryStatBonus   = defaultPrimaryAdditive
                     , _primaryStatMult    = defaultPrimaryMult
                     , _secondaryStatBonus = defaultSecondaryAdditive
                     , _secondaryStatMult  = defaultSecondaryMult
                     -- Procs
                     , _preHitProc         = return ()
                     , _postHitProc        = return ()
                     -}
                     } 

{- Accumulate all the stat mod functions in the right order.  The
 - function-composition opperator needs to be flipped so that they functions
 - are joined in the right order, though they can still be listed in the order
 - they are applied. -}
{-
statModifiers :: Stats -> StatMod
statModifiers stats = 
    foldr (flip (.)) id . map (\f -> getL f stats) $ [ attributeBonus
                                                     , attributeMult
                                                     , primaryStatBonus
                                                     , primaryStatMult
                                                     , secondaryStatBonus
                                                     , secondaryStatMult
                                                     ]

applyStatModifiers :: StatMod
applyStatModifiers stats = statModifiers stats $ stats
-}
