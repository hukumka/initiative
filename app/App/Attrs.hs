{-# Language OverloadedStrings #-}
module App.Attrs 
(
    myAttrMap
)where

import Brick(on, attrMap, fg, AttrName, AttrMap)
import Graphics.Vty as V
import Brick.Widgets.Border(borderAttr)

myAttrMap :: AttrMap
myAttrMap = attrMap defaultAttr 
    $ [(borderAttr, fg grey)]
        <> logAttrs
        <> statsAttrs
        <> initiativeTableAttrs
        <> cmdAttrs

defaultAttr = V.white `on` V.black

statsAttrs = hierarchy "stats" defaultAttr attrs
    where
        attrs = [
                ("attrName", styleAttr V.bold),
                ("trait", styleAttr V.bold),
                ("action", (fg red) <> styleAttr V.bold)
                ]

logAttrs = [("log" <> "link", styleAttr V.bold)]

initiativeTableAttrs = hierarchy "iniTable" mempty selected
    where
        selected = maybeHierarchy "selected" (styleAttr V.bold) units
        units = 
            [
            ("current", fg V.green),
            ("dead", fg $ V.rgbColor 80 0 0),
            ("current" <> "dead", fg V.red)
            ]

cmdAttrs = hierarchy "cmd" (V.black `on` V.white) 
    $ [
        ("completion", fg grey),
        ("error", fg red)
      ]

-- | Creates attribute mapping that contains all input attributes as well as 
-- | attributes combined with given default
maybeHierarchy :: AttrName -> Attr -> [(AttrName, Attr)] -> [(AttrName, Attr)]
maybeHierarchy a d r = r <> hierarchy a d r

hierarchy :: AttrName -> Attr -> [(AttrName, Attr)] -> [(AttrName, Attr)]
hierarchy name default_ attrs = (name, default_) : (map (\(n, v) -> (name <> n, v)) attrs)

styleAttr = V.withStyle mempty

grey = V.rgbColor 80 80 80
