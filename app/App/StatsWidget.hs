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
        phys = (attrName "str: " <+> statWidget U.str)
            <=> (attrName "dex: " <+> statWidget U.dex)
            <=> (attrName "con: " <+> statWidget U.con)
        mental = (attrName "wis: " <+> statWidget U.wis)
            <=> (attrName "int: " <+> statWidget U.int)
            <=> (attrName "cha: " <+> statWidget U.cha)
        rest = (attrName "AC: " <+> attrWidget U.armorClass)
            <=> (attrName "speed: " <+> attrWidget U.speed)
        traits = vBox $ map (colorName $ "stats" <> "trait") $ U.traits c
        extras = vBox $ map str $ U.extras c
        attrName s = withAttr ("stats" <> "attrName") $ str s
        attacks = vBox $ map (colorName $ "stats" <> "action") $ U.attacks c
        attrWidget getter = let attr = getter c
                             in withAttr "stats" $ str $ show attr
        statWidget getter = let attr = getter c
                                bonus = U.attributeBonus attr
                                text = show attr ++ " (" ++ show bonus ++ ")"
                            in withAttr "stats" $ str text

colorName :: AttrName -> String -> Widget n
colorName attr s = let (t:h) = splitOn "." s
                       head = FancyText attr (t ++ ": ")
                       tail = words $ concat $ h
                    in renderFancy (head:tail)
    where
        words = map word . splitOn " "
        word w = FancyText "stats" $ w ++ " "
