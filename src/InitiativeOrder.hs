{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module InitiativeOrder
(
    Unit(..),
    InitiativeOrder,
    prevKey, nextKey, step,
    fromList, update, toList,

    unitL, currentL
) where

import Unit(Unit, initiative)
import qualified Data.Map as M
import Data.List(sortOn, groupBy)
import Lens.Micro.Platform(makeLenses, (%~), (^.), Lens', lens)

-- | (-initiative, idInThisInitiative)
type Key = (Int, Int)

data InitiativeOrder = InitiativeOrder {
    _unitsL :: M.Map Key Unit,
    _currentL :: Int
} 
makeLenses ''InitiativeOrder

step :: InitiativeOrder -> InitiativeOrder 
step io = currentL %~ nextKey io $ io

nextKey :: InitiativeOrder -> Int -> Int
nextKey order k = (k + 1) `mod` (length $ _unitsL order)

prevKey :: InitiativeOrder -> Int -> Int
prevKey order k = (k + l - 1) `mod` l
    where l = length $ _unitsL order

unitL :: Int -> Lens' InitiativeOrder Unit
unitL id = unitsL . lens read update
    where
        read :: M.Map Key Unit -> Unit
        read = snd . M.elemAt id
        update :: M.Map Key Unit -> Unit -> M.Map Key Unit
        update m v = M.updateAt (\_ _ -> Just v) id m


update :: (Unit -> Unit) -> Int -> (InitiativeOrder -> InitiativeOrder)
update f id = unitL id %~ f

fromList :: [Unit] -> InitiativeOrder
fromList u = InitiativeOrder {
                _unitsL = M.fromAscList unitsAndKeys,
                _currentL = 0
             }
    where
        unitsAndKeys = concat 
            $ map tagWithKey
            $ groupBy (\a b -> a ^. initiative == b ^. initiative)
            $ unitsInOrder
        tagWithKey = zipWith (\id v -> ((v ^. initiative, id), v)) [0..]
        unitsInOrder = reverse $ sortOn (^. initiative) u

toList :: InitiativeOrder -> [Unit]
toList = M.elems . _unitsL

