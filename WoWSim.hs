module Main where

import Types.World
import Types.Events
import Types.EntityId

import DisEvSim

import System.Environment
import System.CPUTime

import AI.Warrior
main = do
    [dur]   <- getArgs
    let player = makeEntity "Player"
        target = makeEntity "Target"
    let world = World player target
    print $ simulate world [warrior] EvSimStart (read dur)

