module Actions.Common where

import qualified DisEvSim as Sim
import Types.World
import Control.Monad.Reader


getW = lift Sim.getW
putW = lift . Sim.putW
modW :: (World -> World) -> Action ()
modW = lift . Sim.modW
getT = lift Sim.getT
after t ev = lift $ Sim.after t ev
addHandler :: String -> (Event -> Action ()) -> Action()
addHandler name handler = do
    actionState <- ask
    let h = transformHandler handler actionState
    lift $ Sim.addHandler name h
transformHandler handler state = (flip runReaderT $ state) . handler

resetGCD :: Action ()
resetGCD =
    do  w@(World player _)  <- getW
        t                   <- getT
        let player' = player { eGlobalCD = t + 1.5 }
        putW $ w { player = player' }
        after 1.5 . EvGcdEnd . eID $ player

setCooldown :: String -> Sim.DTime -> Action ()
setCooldown name dt =
    do  w@(World player _) <- getW
        t                  <- getT
        let player' = addCooldown player name (t + dt)
        putW $ w { player = player' }

useAbility :: Ability -> Action ()
useAbility abil = do
    (World player target) <- getW
    t                     <- getT
    if (abilTriggerGCD abil)
        then resetGCD
        else return ()
    case (abilCooldown abil) of
        Nothing -> return ()
        Just dt -> do
            setCooldown (abilName abil) dt
            after dt (EvCooldownExpire (eID player) (abilName abil))
    abilAction abil
