module Actions.Common where

import qualified DisEvSim as Sim
import Types.World
import Control.Monad.Reader

import Data.Functor ((<$>))


getW = lift Sim.getW
putW = lift . Sim.putW
modW :: (World -> World) -> Action ()
modW = lift . Sim.modW
getTime = lift Sim.getT
after t ev = lift $ Sim.after t ev
addHandler :: String -> (Event -> Action ()) -> Action()
addHandler name handler = do
    actionState <- ask
    let h = transformHandler handler actionState
    lift $ Sim.addHandler name h
transformHandler handler state = (flip runReaderT $ state) . handler

getSource = actionSource <$> ask
getTarget = actionTarget <$> ask

insertEntity :: Entity -> Action ()
insertEntity e = do
    w <- getW
    putW $ w { wEntities = updateEntityList e (wEntities w)}

resetGCD :: Action ()
resetGCD =
    do  w <- getW
        t <- getTime
        src  <- getSource
        targ <- getTarget
        let src' = src { eGlobalCD = t + 1.5 }
        insertEntity src'
        after 1.5 . EvGcdEnd . eID $ src

setCooldown :: String -> Sim.DTime -> Action ()
setCooldown name dt =
    do  w <- getW
        t <- getTime
        p <- getSource
        let p' = entityAddCooldown p name (t + dt)
        insertEntity p'

useAbility :: Ability -> Action ()
useAbility abil = do
    w <- getW
    t <- getTime
    p <- getSource
    if (abilTriggerGCD abil)
        then resetGCD
        else return ()
    case (abilCooldown abil) of
        Nothing -> return ()
        Just dt -> do
            setCooldown (abilName abil) dt
            after dt (EvCooldownExpire (eID p) (abilName abil))
    abilAction abil
