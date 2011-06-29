module Actions.Common where

import Prelude hiding (lookup)
import qualified DisEvSim as Sim
import Types.World
import Control.Monad.Reader

import Data.Functor ((<$>))
import Data.Map (lookup, insert, fold, keys)
import Data.Record.Label
import System.Random (StdGen)


-- * Functions on Sim
-- | This function lets us transform handlers defined on Action and lift them to Sim.
makeHandler :: EntityId -> (Event -> Action ()) -> Event -> Sim World Event ()
makeHandler eid a ev = do
    mpEntity <- getEntity eid
    case mpEntity of
        Nothing      -> return ()
        Just pEntity -> (flip runReaderT) (ActionState eid (getL eTarget pEntity)) $ a ev

-- | This loops over all the entities and evaluates any actions they might need evaluated.
execActions :: Event -> Sim World Event ()
execActions (EvAction act) = act
execActions ev = do
    entities <- getL wEntities <$> Sim.getW
    let combined = Data.Map.fold (\e h -> joinHandlers h $ entityActions e) (updateStatsHandler) entities
    combined ev
    where
        entityActions e = makeHandler (getL eID e) . joinHandlers (getL eAI e) . Data.Map.fold (joinHandlers) (const $ return ()) . getL eHandlers $ e
        joinHandlers a b ev = a ev >> b ev

-- | Handle updating from buffs.
updateStatsHandler :: Event -> Sim World Event ()
updateStatsHandler (EvBuffsChanged eid) = updateStats eid
updateStatsHandler (EvSimStart) = do
    w <- Sim.getW
    foldr ((>>) . updateStats) (return ()) . keys . getL wEntities $ w
updateStatsHandler _                    = return ()

-- | Actual update-stats worker.  Does the real work.
updateStats :: EntityId -> Sim World Event ()
updateStats eid = do
    mE <- getEntity eid
    case mE of
        Just e  -> putEntity . setL eStats (buff e . getL eBaseStats $ e) $ e
        Nothing -> return ()
    where
        buff e = foldr (.) id . map (\l -> applyBuffList . getL l $ e) $ [eSecMultBuffs, eSecFlatBuffs, ePriMultBuffs, ePriFlatBuffs, eAttMultBuffs, eAttFlatBuffs]

-- | Get an entity based on the ID.
getEntity :: EntityId -> Sim World Event (Maybe Entity)
getEntity eid = (lookup eid . getL wEntities) `fmap` Sim.getW

putEntity :: Entity -> Sim World Event ()
putEntity e = Sim.modW $ modL wEntities (insert (getL eID e) e)

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
addHandler :: (Show a) => a -> (Event -> Action ()) -> Action()
addHandler name handler = do
    src <- getSource
    modifyEntity (getL eID src) . modL eHandlers $ addHandlerToList (show name) handler

removeHandler :: (Show a) => a -> Action ()
removeHandler name = do
    src <- getSource
    modifyEntity (getL eID src) . modL eHandlers $ removeHandlerFromList (show name)

transformHandler :: (Event -> Action ()) -> Action (Event -> Sim World Event ())
transformHandler h = do 
    actionState <- ask
    return $ (flip runReaderT $ actionState) . h

nullHandler :: Event -> Action()
nullHandler = const . return $ ()

-- ** Manipulate Entities

insertEntity :: Entity -> Action ()
insertEntity e = modW $ modL wEntities (updateEntityList e)

modifyEntity :: EntityId -> (Entity -> Entity) -> Action ()
modifyEntity eid f = modW $ modL wEntities (adjustEntityInList f eid)

getSource :: Action Entity
getSource = do
    eid <- getL actionSource <$> ask
    mE  <- lift $ getEntity eid
    case mE of
        Just e  -> return e
        Nothing -> fail "Could not find source entity."
getTarget :: Action Entity
getTarget = do
    eid <- getL actionTarget <$> ask
    mE  <- lift $ getEntity eid
    case mE of
        Just e  -> return e
        Nothing -> fail "Could not find source entity."

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

setCooldown :: AbilityId -> Sim.DTime -> Action ()
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
    t   <- getTime
    let aid = getL abilName abil
        handlerName = show sid ++ show aid
    modifySource . setL eCast . Just $ (abil, t + dt)
    addHandler handlerName (handler sid)
    after 0  $ EvCastStarted sid aid
    after dt $ EvCastComplete sid aid -- This needs to be removed if the cast is interrupted.
    where
        handler sid (EvCastComplete eid aid) = do
            modifySource . setL eCast $ Nothing
            if (eid == sid) && (getL abilName abil) == aid
                then do getL abilAction abil
                        sid <- getL eID <$> getSource
                        removeHandler (show sid ++ show aid)
                else return ()
        handler _   _ = return ()

addBuff :: ((:->) Entity BuffList) -> BuffId -> Buff -> Action ()
addBuff cat bid buff = modifySource $ modL cat (addBuffToList bid buff)


addSourceAura :: Aura -> Action ()
addSourceAura a = do
    modifySource . modL eAuras . addAuraToList $ a
    sid <- getL eID <$> getSource
    doIn (getL auraDuration a) $ removeSourceAura (getL auraId a)
    after 0 (EvAuraApplied sid sid (getL auraId a))

addTargetAura :: Aura -> Action ()
addTargetAura a = do
    modifyTarget . modL eAuras . addAuraToList $ a
    sid <- getL eID <$> getSource
    tid <- getL eID <$> getTarget
    doIn (getL auraDuration a) $ removeTargetAura (getL auraId a)
    after 0 (EvAuraApplied sid tid (getL auraId a))


removeSourceAura :: AuraId -> Action ()
removeSourceAura aid = do
    modifySource . modL eAuras . removeAuraFromList $ aid
    sid <- getL eID <$> getSource
    after 0 (EvAuraExpire sid sid aid)

removeTargetAura :: AuraId -> Action ()
removeTargetAura aid = do
    modifyTarget . modL eAuras . removeAuraFromList $ aid
    sid <- getL eID <$> getSource
    tid <- getL eID <$> getTarget
    after 0 (EvAuraExpire sid tid aid)

doIn :: DTime -> Action () -> Action ()
doIn dt act = do
    readerst <- ask
    let simAct = runReaderT act readerst
    after dt (EvAction simAct)

doNothing :: Action ()
doNothing = return ()
