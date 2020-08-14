{-# LANGUAGE RankNTypes #-}
module Commands2 
(
    Command,
    CommandSet,
    CommandPick(..),
    buildCommand, buildLogCommand,
    mapCommand, runCommand,
    setFromList, pickCommand
) where

import Lens.Micro.Platform(Lens', (%~), (^.))
import Data.List(stripPrefix)
import Data.Maybe(catMaybes)

data Command s l = Command {
    _name :: String,
    _action :: String -> Either String (s -> IO (s, l))
}

buildLogCommand :: String -> (String -> Either String a) -> (a -> s -> IO l) -> Command s l
buildLogCommand name parse log = Command {
                                    _name = name,
                                    _action = action'
                                }
    where
        action' arg = perform <$> parse arg
        perform repr state = do
            l <- log repr state
            return (state, l)

buildCommand :: String -> (String -> Either String a) 
                -> (a -> s -> IO s) -> (a -> s -> l) -> Command s l
buildCommand name parse action log = Command {
                                        _name = name,
                                        _action = action'
                                     }
    where
        action' arg = perform <$> parse arg
        perform repr state = do
            state' <- action repr state
            return (state', log repr state')

mapCommand :: Lens' s' s -> Command s l -> Command s' l
mapCommand lens command = command { _action = action' }
    where 
        action' args = mapAct <$> (_action command $ args)
        mapAct f s = do
            (s', l) <- f (s ^. lens)
            return (lens %~ const s' $ s, l)

runCommand :: Command s l -> String -> Either String (s -> IO (s, l))
runCommand = _action

data CommandSet s l = CommandSet [Command s l]
data CommandPick s l = CommandPick {
    command :: Command s l,
    prefix :: String,
    syffix :: String,
    args :: String
}

setFromList = CommandSet

pickCommand :: CommandSet s l -> String -> Either String (CommandPick s l)
pickCommand (CommandSet set) line = case words line of
    [] -> Right $ CommandPick (head set) "" (_name $ head set) ""
    (x:rest) -> let l = catMaybes $ map (pick x (unwords rest)) set
                 in case l of
                      (x:rest) -> Right x
                      _ -> Left "No such command"
    where 
        pick x rest command = do
            syffix <- stripPrefix x $ _name command
            return $ CommandPick command x syffix rest
