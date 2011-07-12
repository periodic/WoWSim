module Types.Ability where

import Types.Common
import Types.Stats

import DisEvSim (DTime, Time)
import Data.Record.Label

import Data.Map

realAbilCastTime :: Ability -> Stats -> DTime
realAbilCastTime abil stats = 
    if getL abilSchool abil == Physical
    then actualMeleeCastTime (getL abilCastTime abil) stats
    else actualSpellCastTime (getL abilCastTime abil) stats

getAbilityFromMap :: AbilityId -> AbilityMap -> Maybe Ability
getAbilityFromMap = Data.Map.lookup

addAbilityToMap :: Ability -> AbilityMap -> AbilityMap
addAbilityToMap abil map = insert (getL abilName abil) abil map

emptyAbilityMap = empty
