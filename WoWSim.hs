module Main where

import Types.World

import DisEvSim
import DisEvSim.Debug

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
    let pEntity = makeEntity "Player" "Target" warrior
        tEntity = makeEntity "Target" "Player" nullHandler
        pID     = getL eID pEntity
        entities = addEntityList pEntity . addEntityList tEntity $ empty
        -- TODO: move this to Types.World
        world   = World { _wEntities = entities
                        , _wGen      = gen
                        }
        config  = defaultConfig { enableLog = True }
        (t, log, world') = {-# SCC "sim" #-} simulate config world [("Core Handler", execActions)] EvSimStart (read dur)
    putStrLn . showLog $ log
    let tFinal = Map.lookup (EntityId "Target") . getL wEntities $ world'
    case tFinal of
        Just targ -> print $ (t, getL eHealth targ)
        Nothing   -> print $ (t)

showLog :: [(Time, Event)] -> String
showLog = intercalate "\n" . map (\(t,e) -> show t ++ " - " ++ show e)
