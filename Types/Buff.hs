module Types.Buff where

import Types.Common

import Data.Map

emptyBuffList :: BuffList
emptyBuffList = empty

addBuffToList :: BuffId -> Buff -> BuffList -> BuffList
addBuffToList = insert

removeBuffFromList :: BuffId -> BuffList -> BuffList
removeBuffFromList = delete

applyBuffList :: BuffList -> Stats -> Stats
applyBuffList = Data.Map.fold (.) id
