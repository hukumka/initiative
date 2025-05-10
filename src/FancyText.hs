module FancyText 
(
    FancyText(..), renderFancy   
) where 

import Brick
import Data.String hiding (lines)
import Data.List(foldl')
import Prelude hiding (lines)

data FancyText = FancyText AttrName String

instance IsString FancyText where
    fromString = FancyText mempty

renderFancy :: [FancyText] -> Widget n
renderFancy x = Widget Greedy Fixed $ do
    c <- getContext
    let lines = splitByWidth (availWidth c) x
     in render $ vBox $ map renderLine lines
    where 
        renderLine = hBox . map renderBlock
        renderBlock (FancyText attr s) = withAttr attr $ str s

splitByWidth :: Int -> [FancyText] -> [[FancyText]]
splitByWidth width blocks = extract $ foldl' addBlock (splitter width) blocks

data Splitter = Splitter { offset :: Int, width :: Int, lines :: [[FancyText]], line :: [FancyText] }
splitter :: Int -> Splitter
splitter width = Splitter { offset = 0, width = width, lines = [], line = [] }

extract :: Splitter -> [[FancyText]]
extract splitter' = reverse $ map reverse $ line splitter' : lines splitter'

addBlock :: Splitter -> FancyText -> Splitter
addBlock splitter' block = 
    let (FancyText _ s) = block
     in if offset splitter' + textWidth s > width splitter'
        then splitter' { 
            lines = line splitter' : lines splitter',
            line = [block],
            offset = textWidth s
        }
        else splitter' {
            line = block:line splitter',
            offset = offset splitter' + textWidth s
        }
