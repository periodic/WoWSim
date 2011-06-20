module Actions.Common where

import Prelude hiding (lookup)
import qualified DisEvSim as Sim
import Types.World
import Control.Monad.Reader

import Data.Functor ((<$>))
import Data.Map (lookup)


-- * Functions on Sim
-- | This function lets us transform handlers defined on Action and lift them to Sim.
makeHandler :: Entity -> (Event -> Action ()) -> Event -> Sim World Event ()
makeHandler e a ev = do
    mpEntity <- getEntity . eID $ e
    case mpEntity of
        Nothing      -> return ()
        Just pEntity -> do
            mtEntity <- getEntity . eTarget $ pEntity
            case mtEntity of 
                Nothing      -> return ()
                Just tEntity -> (flip runReaderT) (ActionState pEntity tEntity) $ a ev

-- | Get an entity based on the ID.
getEntity :: EntityId -> Sim World Event (Maybe Entity)
getEntity eid = (lookup eid . wEntities) `fmap` Sim.getW

-- * Functions on Action
-- ** Data manipulation functions
getW = lift Sim.getW
putW = lift . Sim.putW
modW :: (World -> World) -> Action ()
modW = lift . Sim.modW
getTime = lift Sim.getT
after t ev = lift $ Sim.after t ev
getSource = actionSource <$> ask
getTarget = actionTarget <$> ask

-- ** Other functions
addHandler :: String -> (Event -> Action ()) -> Action()
addHandler name handler = do
    actionState <- ask
    h <- transformHandler handler
    lift $ Sim.addHandler name h

transformHandler :: (Event -> Action ()) -> Action (Event -> Sim World Event ())
transformHandler h = do 
    actionState <- ask
    return $ (flip runReaderT $ actionState) . h

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
