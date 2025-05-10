{-# LANGUAGE OverloadedStrings #-}
module App.StatsWidget 
(
    renderStatWidget
) where

import qualified Unit as U
import Data.List.Split(splitOn)
import Brick
import FancyText
import Brick.Widgets.Border(borderWithLabel)

renderStatWidget :: U.Unit -> Widget n
renderStatWidget u = borderWithLabel (str $ U._name u) (renderStatWidget' $ U._extra u)

renderStatWidget' :: U.UnitConfig -> Widget n
renderStatWidget' c = (phys <+> (str " ") <+> mental <+> (str " ") <+> rest)
        <=> extras
        <=> traits
        <=> attacks
    where 
        phys = (attrNameWidget "str: " <+> statWidget U.str)
            <=> (attrNameWidget "dex: " <+> statWidget U.dex)
            <=> (attrNameWidget "con: " <+> statWidget U.con)
        mental = (attrNameWidget "wis: " <+> statWidget U.wis)
            <=> (attrNameWidget "int: " <+> statWidget U.int)
            <=> (attrNameWidget "cha: " <+> statWidget U.cha)
        rest = (attrNameWidget "AC: " <+> attrWidget U.armorClass)
            <=> (attrNameWidget "speed: " <+> attrWidget U.speed)
        traits = vBox $ map (colorName $ attrName "stats" <> attrName "trait") $ U.traits c
        extras = vBox $ map str $ U.extras c
        attrNameWidget s = withAttr (attrName "stats" <> attrName "attrName") $ str s
        attacks = vBox $ map (colorName $ attrName "stats" <> attrName "action") $ U.attacks c
        attrWidget getter = let attr = getter c
                             in withAttr (attrName "stats") $ str $ show attr
        statWidget getter = let attr = getter c
                                bonus = U.attributeBonus attr
                                text = show attr ++ " (" ++ show bonus ++ ")"
                            in withAttr (attrName "stats") $ str text

colorName :: AttrName -> String -> Widget n
colorName attr s = let (t:h) = splitOn "." s
                       head' = FancyText attr (t ++ ": ")
                       tail' = words' $ concat $ h
                    in renderFancy (head':tail')
    where
        words' = map word . splitOn " "
        word w = FancyText (attrName "stats") $ w ++ " "
