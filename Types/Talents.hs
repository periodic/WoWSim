{- * Talents
 -}
module Types.Talents where

import Control.Monad.Reader
import Data.Record.Label
import qualified Data.Map 
import DisEvSim (Sim, getW)

import Types.Common
import Types.Entity

applyTalents :: TalentMap -> TalentList -> EntityId -> Sim World Event ()
applyTalents tmap talents sid = do
    w <- getW
    case lookupEntityById sid . getL wEntities $ w of
        Nothing      -> return ()
        Just pEntity -> sequence_ $ applyList (getL eID pEntity) (getL eTarget pEntity) tmap talents
    where
        talent2Sim :: EntityId -> EntityId -> Integer -> (Integer -> Action ()) -> Sim World Event ()
        talent2Sim sid tid i t = (flip runReaderT) (ActionState sid tid) $ t i
        applyList :: EntityId -> EntityId -> TalentMap -> TalentList -> [Sim World Event ()]
        applyList sid tid tmap = map (\(p, t) -> maybe (return ()) (talent2Sim sid tid p . getL talentAction) (lookupTalent t tmap))


lookupTalent :: TalentId -> TalentMap -> Maybe Talent
lookupTalent = Data.Map.lookup

