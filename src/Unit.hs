{-# LANGUAGE TemplateHaskell #-}
module Unit 
(
    Unit, UnitG(..), UnitConfig(..),
    name, initiative, health, maxHealth, extra,
    isAlive, isDead, dealDamage,
    mkUnit,
    attributeBonus
)
where

import Lens.Micro.Platform(makeLenses)
import Data.Aeson.TH

type Unit = UnitG UnitConfig
mkUnit n i h = UnitG n i h h ()

data UnitG a = UnitG {
    _name :: String,
    _initiative :: Int,
    _health :: Int,
    _maxHealth :: Int,
    _extra :: a
}

data UnitConfig = UnitConfig {
    str :: Int,
    dex :: Int,
    con :: Int,
    wis :: Int,
    int :: Int,
    cha :: Int,
    armorClass :: Int,
    speed :: String,
    attacks :: [String],
    traits :: [String],
    extras :: [String]
}

$(deriveJSON defaultOptions{fieldLabelModifier=drop 1} ''UnitG)
$(deriveJSON defaultOptions{
    fieldLabelModifier = \x -> case x of
                               "armorClass" -> "armor"
                               "attacks" -> "actions"
                               x -> x
} ''UnitConfig)

makeLenses ''UnitG

isAlive unit = _health unit > 0
isDead unit = _health unit <= 0
dealDamage x unit = unit {_health = _health unit - x}

attributeBonus :: Int -> Int
attributeBonus x = x `div` 2 - 5
