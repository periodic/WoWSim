module Types.Aura where

import Types.Common

import Data.Map
import Data.Record.Label

import Text.Printf

addAuraToMap :: Aura -> AuraMap -> AuraMap
addAuraToMap aura = insert (getL auraId aura) aura

removeAuraFromMap :: AuraId -> AuraMap -> AuraMap
removeAuraFromMap = delete

isAuraInMap :: AuraId -> AuraMap -> Bool
isAuraInMap = member

emptyAuraMap = empty

