{- * Talents
 -}
module Types.Talents where

import Control.Monad.Reader
import Data.Record.Label
import qualified Data.Map 
import DisEvSim (Sim, getW)

import Types.Common
import Types.Entity

applyTalents :: TalentMap -> TalentList -> Entity -> Entity
applyTalents tmap talents e = 
    let efunc = foldr (.) id . map (uncurry applyTalent) $ talents
     in efunc e
    where
        applyTalent :: TalentId -> Integer -> Entity -> Entity
        applyTalent tid p = maybe (id)
                                  (\t -> (getL talentEffect t) p)
                                  (lookupTalent tid tmap)


lookupTalent :: TalentId -> TalentMap -> Maybe Talent
lookupTalent = Data.Map.lookup

