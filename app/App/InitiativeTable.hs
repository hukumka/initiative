{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module App.InitiativeTable
(
    TableState, 
    handleTableEvent, 
    renderInitiativeTable,
    fromList,
    currentUnitL, selectedUnitL, order,
    commands
) where

import FancyText(FancyText(..))
import Commands2(buildCommand, buildLogCommand)
import Text.Read(readEither)
import DiceRoll
import Table(renderTable, row)
import Lens.Micro.Platform((^.), (%~), (&), makeLenses, Lens')
import qualified InitiativeOrder as I
import qualified Unit as U
import qualified Graphics.Vty as V
import Brick
import App.CommandWidget(Command)

data TableState = TableState {
    _order :: I.InitiativeOrder,
    _selected :: Int
}
makeLenses ''TableState

currentUnitL :: Lens' TableState U.Unit
currentUnitL f state = 
    let i = state ^. order . I.currentL
        pick = order . I.unitL i
    in pick f state

selectedUnitL :: Lens' TableState U.Unit
selectedUnitL f state = 
    let i = state ^. selected :: Int
        pick = order . I.unitL i
    in pick f state

fromList :: [U.Unit] -> TableState
fromList u = TableState (I.fromList u) 0

handleTableEvent :: TableState -> BrickEvent n e -> EventM n (Next TableState)
handleTableEvent state event = case event of
    VtyEvent (V.EvKey V.KUp []) -> continue $ selectPrev
    VtyEvent (V.EvKey V.KDown []) -> continue $ selectNext
    VtyEvent (V.EvKey (V.KChar 'n') []) -> continue $ step
    _ -> continue state
    where
        selectPrev = state & selected %~ I.prevKey (_order state)
        selectNext = state & selected %~ I.nextKey (_order state)
        step = state & order %~ I.step

renderInitiativeTable :: TableState -> Widget n
renderInitiativeTable dat = renderList $ zip [0..] $ I.toList $ order'
    where 
        order' = _order dat
        isCurrent i = i == (order' ^. I.currentL)
        renderList = renderTable [
                        row (display U._name) "Name", 
                        row (display $ show . U._initiative) "Initiative", 
                        row (display $ hp) "Health" 
                        ]
        hp u = (show $ U._health u) ++ "/" ++ (show $ U._maxHealth u)
        display :: (U.Unit -> String) -> (Int, U.Unit) -> Widget n
        display field (i, unit) = withAttr attr $ str $ field unit
            where attr = "iniTable" <> sel <> curr <> dead
                  curr = condAttr (isCurrent i) "current"
                  dead = condAttr (U.isDead unit) "dead"
                  sel = condAttr (i == _selected dat) "selected"
                  condAttr cond attr' = if cond then attr' else mempty

commands :: [Command TableState]
commands = [nextTurn, damage, heal, roll]
    where
        damage = buildCommand "damage" readEither (heal' . \x -> -x) damageLog
        damageLog x s = (current s) : " deals " : (fancy x) : " points of damage to " : (selected' s) : []

        heal = buildCommand "heal" readEither heal' healLog
        heal' x s = pure $ selectedUnitL . U.health %~ (+x) $ s
        healLog x s = (current s) : " healed " : (selected' s) : " for " : (fancy x) : " points" : []

        nextTurn = buildCommand "nextTurn" (Right . const ()) nextTurn' nextTurnLog
        nextTurn' () s = pure $ order . I.currentL %~ (I.nextKey $ s ^. order) $ s
        nextTurnLog () s = "Now it is " : (current s) : "'s turn" : []

        roll = buildLogCommand "roll" parseRoll logRoll'
        logRoll' r s = do
            res <- runRoll r
            return $ logRoll r s res
        logRoll r s res = (current s) : " rolls " : (fancy r) : ": " : (fancy res) : []
        
        current s = unitMark (s ^. currentUnitL) :: FancyText
        selected' s = unitMark (s ^. selectedUnitL)
        unitMark u = FancyText ("log" <> "link") (u ^. U.name)
        fancy x = FancyText ("log" <> "link") (show x)

