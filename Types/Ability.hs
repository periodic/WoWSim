module Types.Ability ( realAbilCastTime
                     ) where

import Types.Common
import Types.Stats

import DisEvSim (DTime, Time)
import Data.Record.Label

realAbilCastTime :: Ability -> Stats -> DTime
realAbilCastTime abil stats = 
    if getL abilSchool abil == Physical
    then actualMeleeCastTime (getL abilCastTime abil) stats
    else actualSpellCastTime (getL abilCastTime abil) stats
