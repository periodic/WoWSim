module Types.Aura where

import Types.Common

import Data.Map
import Data.Record.Label

import Text.Printf

addAuraToList :: Aura -> AuraList -> AuraList
addAuraToList aura = insert (getL auraId aura) aura

removeAuraFromList :: AuraId -> AuraList -> AuraList
removeAuraFromList = delete

isAuraInList :: AuraId -> AuraList -> Bool
isAuraInList = member

emptyAuraList = empty

