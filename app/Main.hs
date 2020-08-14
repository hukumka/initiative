{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# Language OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class(liftIO)
import Lens.Micro.Platform((^.), (%~), makeLenses, Lens')
import System.Environment(getArgs)

import Prelude hiding (log)

import Brick
import qualified Graphics.Vty as V
import App.Attrs(myAttrMap)

import qualified App.InitiativeTable as IT
import qualified App.CommandWidget as CW
import qualified App.StatsWidget as SW
import App.Log
import qualified Unit as U
import qualified Commands2
import Data.Yaml(decodeFileThrow)
import DiceRoll

data AppState = AppState {
    _cmd :: CW.CommandState,
    _initiative :: IT.TableState,
    _lastRoll :: Int,
    _log :: Log
}
makeLenses ''AppState

main :: IO ()
main = do
    state <- initialState
    const () <$> defaultMain app state
    where
        app = App {
                appDraw = (:[]) . renderApp :: AppState -> [Widget ()],
                appChooseCursor = (\_ _ -> Nothing),
                appAttrMap = const myAttrMap,
                appHandleEvent = handleEvent,
                appStartEvent = return
              }

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent state event = case event of
    VtyEvent (V.EvKey V.KEsc []) -> halt state
    _ -> let CW.EventH s mc = CW.handleCmdEvents commands (_cmd state) event
             state' = state {_cmd = s}
          in case mc of
                Just c -> do
                   (state'', l) <- liftIO $ c state'
                   continue $ log %~ (append l) $ state''
                Nothing -> lensEvent initiative IT.handleTableEvent state' event

renderApp :: AppState -> Widget ()
renderApp state = (table <+> leftPanel) <=> (padTop Max $ logWidget) <=> cmd'
    where
        leftPanel = currentStats <=> selectedStats
        table = IT.renderInitiativeTable $ _initiative state
        cmd' = CW.renderCommandWidget commands (_cmd state)
        stats pick = SW.renderStatWidget $ state ^. (initiative . pick)
        currentStats = stats IT.currentUnitL
        selectedStats = stats IT.selectedUnitL
        logWidget = renderLog $ _log state

initialState :: IO AppState
initialState = do
    units <- readUnits
    return $ AppState CW.initState (IT.fromList units) 0 emptyLog

-- | Commands available for cmd widget
data Command = ModifyHealth Int 
             | NextTurn
             | Roll DiceRoll

commands :: Commands2.CommandSet AppState Record
commands = Commands2.setFromList $ 
    map (Commands2.mapCommand initiative) IT.commands

readUnits :: IO [U.Unit]
readUnits = do
    configFile <- head <$> getArgs
    config <- decodeFileThrow configFile :: IO [U.UnitG FilePath]
    mapM applyConfig config
    where
        applyConfig :: U.UnitG FilePath -> IO (U.UnitG U.UnitConfig)
        applyConfig u = do
            config <- getUnitConfig $ u ^. U.extra
            return $ U.extra %~ const config $ u
        getUnitConfig = decodeFileThrow :: FilePath -> IO U.UnitConfig

type EventHandler a n e = (a -> BrickEvent n e -> EventM n (Next a))

lensEvent :: Lens' b a -> EventHandler a n e -> EventHandler b n e
lensEvent l handler state event = (update <$>) <$> handler (state ^. l) event
    where
        update x = l %~ const x $ state
