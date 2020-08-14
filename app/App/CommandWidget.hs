{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module App.CommandWidget 
(
    renderCommandWidget,
    handleCmdEvents,
    EventH(..),
    Command, CommandState, initState
) where

import Brick
import qualified Commands2 as C
import Data.Maybe(fromMaybe)
import qualified Graphics.Vty as V
import App.Log(Record)
import Lens.Micro.Platform

data EventH s = EventH CommandState (Maybe (s -> IO (s, Record)))
type Command s = C.Command s Record
type CommandSet s = C.CommandSet s Record

data CommandState = CommandState{
    _input :: String,
    _err :: String
}
makeLenses ''CommandState
initState = CommandState "" ""

update x = EventH x Nothing

renderCommandWidget :: CommandSet s -> CommandState -> Widget n
renderCommandWidget commands inp = withAttr "cmd" $ padRight Max
        $ case pick of
            Right p -> definite p
            Left e -> str (_input inp) <+> err' e
    where
        pick = C.pickCommand commands (inp ^. input)
        definite pick = 
            str prefix
            <+> completion
            <+> str (" " ++ args)
            <+> err' (inp ^. err)
            where
                C.CommandPick{
                    C.syffix=syffix, 
                    C.prefix=prefix, 
                    C.args=args
                } = pick
                completion = withAttr ("cmd" <> "completion") $ str syffix
        err' msg = withAttr ("cmd" <> "error") $ padLeft Max $ str msg

handleCmdEvents :: CommandSet s -> CommandState -> BrickEvent n e -> EventH s
handleCmdEvents commands inp e = 
    case e of
        VtyEvent (V.EvKey (V.KChar c) []) -> update $ (input %~ (++ [c])) . (err %~ const "") $ inp 
        VtyEvent (V.EvKey V.KBS []) -> update $ input %~ init $ inp
        VtyEvent (V.EvKey V.KEnter []) -> 
            let x = do
                        C.CommandPick{C.command=command, C.args=args} <- C.pickCommand commands (inp ^. input)
                        C.runCommand command args
            in case x of
                Right c -> EventH (input %~ const "" $ inp) $ Just c
                Left e -> update $ err %~ const e $ inp
        _ -> update $ inp
