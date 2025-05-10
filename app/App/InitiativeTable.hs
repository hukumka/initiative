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

handleTableEvent :: BrickEvent n e -> EventM n TableState ()
handleTableEvent event = do
    state <- get
    case event of
        VtyEvent (V.EvKey V.KUp []) -> put $ selectPrev state
        VtyEvent (V.EvKey V.KDown []) -> put $ selectNext state
        _ -> return ()
    where
        selectPrev state = state & selected %~ I.prevKey (_order state)
        selectNext state = state & selected %~ I.nextKey (_order state)

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
            where attr = (attrName "iniTable") <> sel <> curr <> dead
                  curr = condAttr (isCurrent i) (attrName "current")
                  dead = condAttr (U.isDead unit) (attrName "dead")
                  sel = condAttr (i == _selected dat) (attrName "selected")
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
        unitMark u = FancyText (attrName "log" <> attrName "link") (u ^. U.name)
        fancy x = FancyText (attrName "log" <> attrName "link") (show x)

