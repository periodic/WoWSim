module Main where

import Types.World
import Types.EntityId

import DisEvSim

import System.Environment
import System.CPUTime

import AI.AutoAttack
import AI.Warrior

main = do
    [dur]   <- getArgs
    let pEntity = makeEntity "Player"
        tEntity = makeEntity "Target"
    let world = World pEntity tEntity
        (t, log, world') = {-# SCC "sim" #-} simulate world [warrior] EvSimStart (read dur)
    print $ (t, eHealth . target $ world')

