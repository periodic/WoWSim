module Actions.Common where

import Prelude hiding (lookup)
import qualified DisEvSim as Sim
import Types.World
import Control.Monad.Reader

import Data.Functor ((<$>))
import Data.Map (lookup)
import Data.Record.Label
import System.Random (StdGen)


-- * Functions on Sim
-- | This function lets us transform handlers defined on Action and lift them to Sim.
makeHandler :: Entity -> (Event -> Action ()) -> Event -> Sim World Event ()
makeHandler e a ev = do
    mpEntity <- getEntity . (getL eID) $ e
    case mpEntity of
        Nothing      -> return ()
        Just pEntity -> do
            mtEntity <- getEntity . (getL eTarget) $ pEntity
            case mtEntity of 
                Nothing      -> return ()
                Just tEntity -> (flip runReaderT) (ActionState pEntity tEntity) $ a ev

-- | Get an entity based on the ID.
getEntity :: EntityId -> Sim World Event (Maybe Entity)
getEntity eid = (lookup eid . getL wEntities) `fmap` Sim.getW

-- * Functions on Action
-- ** General utilities

withRandom :: (StdGen -> (a, StdGen)) -> Action a
withRandom f  = do
    gen <- getL wGen <$> getW
    let (res, gen') = (f gen)
    modW $ setL wGen gen'
    return res

-- ** Lifters from Sim to Action
getW :: Action World
getW = lift Sim.getW
putW :: World -> Action ()
putW = lift . Sim.putW
modW :: (World -> World) -> Action ()
modW = lift . Sim.modW
getTime :: Action Time
getTime = lift Sim.getT
after :: DTime -> Event -> Action ()
after t ev = lift $ Sim.after t ev

-- ** Handlers Utilities
addHandler :: String -> (Event -> Action ()) -> Action()
addHandler name handler = do
    actionState <- ask
    h <- transformHandler handler
    lift $ Sim.addHandler name h

removeHandler :: String -> Action ()
removeHandler name = do
    lift $ Sim.removeHandler name

transformHandler :: (Event -> Action ()) -> Action (Event -> Sim World Event ())
transformHandler h = do 
    actionState <- ask
    return $ (flip runReaderT $ actionState) . h

-- ** Manipulate Entities

insertEntity :: Entity -> Action ()
insertEntity e = modW $ modL wEntities (updateEntityList e)

modifyEntity :: EntityId -> (Entity -> Entity) -> Action ()
modifyEntity eid f = modW $ modL wEntities (adjustEntityInList f eid)

getSource :: Action Entity
getSource = getL actionSource <$> ask
getTarget :: Action Entity
getTarget = getL actionTarget <$> ask

modifySource :: (Entity -> Entity) -> Action ()
modifySource f = do
    sid <- getL eID <$> getSource
    modW $ modL wEntities (adjustEntityInList f sid)
modifyTarget :: (Entity -> Entity) -> Action ()
modifyTarget f = do
    tid <- getL eID <$> getTarget
    modW $ modL wEntities (adjustEntityInList f tid)

-- ** Dealing with abilities
getAbilCastTime :: Ability -> Action DTime
getAbilCastTime abil = do
    stats <- getL eStats <$> getSource
    return $ realAbilCastTime abil stats

-- ** Manipulate the source entity
resetGCD :: Action ()
resetGCD =
    do  t <- getTime
        modifySource (setL eGlobalCD (t + 1.5))
        src  <- getSource
        after 1.5 . EvGcdEnd . getL eID $ src

setCooldown :: String -> Sim.DTime -> Action ()
setCooldown name dt =
    do  t <- getTime
        src <- getSource
        modifySource (entityAddCooldown name (t + dt))
        after dt (EvCooldownExpire (getL eID src) name)

useAbility :: Ability -> Action ()
useAbility abil = do
    if (getL abilTriggerGCD abil)
        then resetGCD
        else return ()
    case (getL abilCooldown abil) of
        Nothing -> return ()
        Just dt -> setCooldown (getL abilName abil) dt
    ct <- getAbilCastTime abil
    if ct <= 0
        then getL abilAction abil
        else registerCast ct abil

registerCast :: DTime -> Ability -> Action ()
registerCast dt abil= do
    sid <- getL eID <$> getSource
    let aid = getL abilName abil
        handlerName = show sid ++ aid
    addHandler handlerName (handler sid)
    after dt $ EvCastComplete sid aid
    where
        handler sid (EvCastComplete eid aid) =
            if (eid == sid) && (getL abilName abil) == aid
            then do getL abilAction abil
                    sid <- getL eID <$> getSource
                    removeHandler (show sid ++ aid)
            else return ()
        handler _   _ = return ()
    
