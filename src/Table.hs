{-# Language OverloadedStrings #-}
module Table
( 
    TableField, 
    row, rowStr, rowShow,
    renderTable
) where

import Data.List(intersperse)
import Graphics.Vty.Image(imageWidth)
import Brick
import Brick.Widgets.Center(hCenter)
import Brick.Widgets.Border(hBorderWithLabel, hBorder, joinableBorder, borderAttr)

-- | Single row of table header.
data TableField s n = TableField {
    pickValue :: s -> Widget n,
    label :: String
}

{- |
    Describe single table column.
    `fn` - function selecting particular field.
    `label` - Label of column to be displayed in header.
    `width` - Width of the column
-}
rowShow :: (Show a) => (s -> a) -> String -> TableField s n
rowShow fn = TableField (str . show . fn) 

row :: (s -> Widget n) -> String -> TableField s n
row = TableField

rowStr :: (s -> String) -> String -> TableField s n
rowStr fn = TableField (str . fn)

{- |
    Given header description (`columns`) and list of rows (`table`) create widget
    for corresponding table.
    
-}
renderTable :: [TableField s n] -> [s] -> Widget n
renderTable columns table = hBox [
                                  border False True,
                                  hBox $ intersperse (border True True) $ map columnWidget' columns,
                                  border True False
                                 ]
    where
        columnWidget c = vBox $ label' c : separatedRows c
        label' c = hBorderWithLabel $ hBox [
                        joinableBorder $ Edges True True True False,
                        withAttr borderAttr $ str $ label c,
                        joinableBorder $ Edges True True False True
                       ]
        separatedRows c =  (<=> hBorder) . pickValue c <$> table
        columnWidget' c = Widget Fixed Fixed $ do
            ctx <- getContext
            images <- monadList $ (render . (pickValue c) <$> table)
            let width = maximum $ map (imageWidth . image) images
                width' = max width $ 2 + length (label c)
             in render $ hLimit width'
                       $ columnWidget c
        border notFirst notLast = vBox [
                joinableBorder $ Edges False True notFirst notLast,
                vBox $ take ((length table) - 1) $ repeat middle,
                joinableBorder $ Edges True True False False,
                joinableBorder $ Edges True False notFirst notLast 
            ]
            where middle = (joinableBorder $ Edges True True False False) 
                           <=> (joinableBorder $ Edges True True notFirst notLast) 

monadList :: (Monad m) => [m a] -> m [a]
monadList = foldl step (return [])
    where 
        step a b = flip (:) <$> a <*> b
