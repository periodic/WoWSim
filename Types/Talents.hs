{- * Talents
 -}
module Types.Talents where

import Control.Monad.Reader
import Data.Record.Label
import qualified Data.Map 
import DisEvSim (Sim, getW)

import Types.Common
import Types.Entity

applyTalents :: TalentMap -> TalentList -> EntityId -> World -> World
applyTalents tmap talents sid w =
    case lookupEntityById sid . getL wEntities $ w of
        Nothing      -> w
        Just pEntity -> (foldr (.) id 
                      . map (uncurry applyTalent) $ talents)
                      $ w
    where
        applyTalent :: TalentId -> Integer -> World -> World
        applyTalent tid p = maybe (id)
                                  (\t -> (getL talentEffect t) sid p)
                                  (lookupTalent tid tmap)


lookupTalent :: TalentId -> TalentMap -> Maybe Talent
lookupTalent = Data.Map.lookup

