{-# LANGUAGE FlexibleInstances #-}
module App.Log
(
    Log, renderLog, append, emptyLog, Record
) where

import FancyText
import Brick
import Brick.Widgets.List
import Brick.Widgets.Border(borderWithLabel)

data Log = Log [Record]

type Record = [FancyText]

emptyLog :: Log 
emptyLog = Log []

append :: Record -> Log -> Log 
append r (Log x) = Log (r:x)

renderLog :: Log -> Widget n
renderLog (Log l) = borderWithLabel (str "Log") 
    $ vBox $ (padRight Max . renderRecord) <$> (take maxLen l)

maxLen = 20

renderRecord :: Record -> Widget n
renderRecord = renderFancy
