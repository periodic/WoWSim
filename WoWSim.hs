module Main where

import Types.World
import Types.EntityId

import DisEvSim

import System.Environment
import System.CPUTime

import Data.List (intercalate)

import Actions.Common
import AI.AutoAttack
import AI.Warrior

main = do
    [dur]   <- getArgs
    let pEntity = makeEntity "Player"
        tEntity = makeEntity "Target"
        world   = World pEntity tEntity
        ai      = transformHandler (warrior (eID pEntity)) (ActionState pEntity tEntity)
        (t, log, world') = {-# SCC "sim" #-} simulate world [("Warrior", ai)] EvSimStart (read dur)
    putStrLn . showLog $ log
    print $ (t, eHealth . target $ world')

showLog = intercalate "\n" . map (\(t,e) -> show t ++ " - " ++ show e)
