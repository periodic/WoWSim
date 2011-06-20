module Main where

import Types.World
import Types.EntityId

import DisEvSim

import System.Environment
import System.CPUTime
import System.Random

import Data.List (intercalate)
import Data.Map as Map (empty, lookup)

import Actions.Common
import AI.AutoAttack
import AI.Warrior

main = do
    [dur]   <- getArgs
    gen     <- newStdGen
    let pEntity = makeEntity "Player"
        tEntity = makeEntity "Target"
        entities = addEntityList pEntity . addEntityList tEntity $ empty
        world   = World { wEntities = entities 
                        , wGen    = gen
                        }
        ai      = makeHandler pEntity warrior
        config  = defaultConfig { enableLog = True }
        (t, log, world') = {-# SCC "sim" #-} simulate config world [("Warrior", ai)] EvSimStart (read dur)
    putStrLn . showLog $ log
    print $ (t, Map.lookup (getIdFromString "Target") . wEntities $ world')

showLog = intercalate "\n" . map (\(t,e) -> show t ++ " - " ++ show e)
