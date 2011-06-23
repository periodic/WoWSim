module Types.Aura where

import Types.Common

import Data.Map
import Data.Record.Label

import Text.Printf

addAuraToMap :: Aura -> AuraMap -> AuraMap
addAuraToMap aura = insert (getL auraId aura) aura

removeAuraFromMap :: AuraId -> AuraMap -> AuraMap
removeAuraFromMap = undefined

isAuraInMap :: AuraId -> AuraMap -> Bool
isAuraInMap = undefined

instance Show Aura where
    show (Aura id school typ cat f) = printf "Aura { id = %s, school = %s, type = %s, category = %s}" (show id) (show school) (show typ) (show cat)
