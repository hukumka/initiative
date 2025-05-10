{-# Language OverloadedStrings #-}
module App.Attrs 
(
    myAttrMap
)where

import Brick(on, attrMap, fg, AttrName, AttrMap, attrName)
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

statsAttrs = hierarchy (attrName "stats") defaultAttr attrs
    where
        attrs = [
                (attrName "attrName", styleAttr V.bold),
                (attrName "trait", styleAttr V.bold),
                (attrName "action", (fg red) `V.withStyle` V.bold)
                ]

logAttrs = [(attrName "log" <> attrName "link", styleAttr V.bold)]

initiativeTableAttrs = hierarchy (attrName "iniTable") defAttr selected
    where
        selected = maybeHierarchy (attrName "selected") (styleAttr V.bold) units
        units = 
            [
            (attrName "current", fg V.green),
            (attrName "dead", fg $ V.rgbColor 80 0 0),
            (attrName "current" <> attrName "dead", fg V.red)
            ]

cmdAttrs = hierarchy (attrName "cmd") (V.black `on` V.white) 
    $ [
        (attrName "completion", fg grey),
        (attrName "error", fg red)
      ]

-- | Creates attribute mapping that contains all input attributes as well as 
-- | attributes combined with given default
maybeHierarchy :: AttrName -> Attr -> [(AttrName, Attr)] -> [(AttrName, Attr)]
maybeHierarchy a d r = r <> hierarchy a d r

hierarchy :: AttrName -> Attr -> [(AttrName, Attr)] -> [(AttrName, Attr)]
hierarchy name default_ attrs = (name, default_) : (map (\(n, v) -> (name <> n, v)) attrs)

styleAttr :: Style -> Attr
styleAttr = V.withStyle currentAttr

grey = V.rgbColor 80 80 80
