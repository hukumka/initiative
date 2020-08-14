module DiceRoll
(
    DiceRoll,
    parseRoll,
    runRoll, runRoll'
) where

import Text.Parsec
import Text.Parsec.Prim
import System.Random(randomRIO)
import Data.Functor.Identity
import Data.List(intersperse)

data DiceRoll = DiceRoll [(Int, Int)]

instance Show DiceRoll where
    show (DiceRoll x) = concat $ intersperse " + " $ map showOne x
        where
            showOne (x, 1) = (show x)
            showOne (1, x) = 'd':(show x)
            showOne (c, x) = (show c) ++ "d" ++ (show x)

type Parsec' a = Parsec String () a

runRoll :: DiceRoll -> IO Int
runRoll dr = sum <$>  runRoll' dr

runRoll' (DiceRoll rl) = mapM rollSet rl

rollSet :: (Int, Int) -> IO Int
rollSet (count, die) = sum <$> (mapM (rand 1) $ take count $ repeat die)
    where rand x y = randomRIO (x, y)

parseRoll :: String -> Either String DiceRoll
parseRoll s = case parse parser "" s of
                Left x -> Left "Error parsing roll string."
                Right x -> Right x

parser :: Parsec' DiceRoll
parser = do
        x <- parseRoll'
        rest <- many (separator >> parseRoll')
        return $ DiceRoll (x:rest)
    where
        separator = skipMany space >> char '+' >> skipMany space

parseRoll' :: Parsec' (Int, Int)
parseRoll' = parseDie <|> (try parseMultiDie) <|> parseConst
    where 
        parseMultiDie = do
            x <- integer
            (_, die) <- parseDie
            return (x, die)

parseConst :: Parsec' (Int, Int)
parseConst = do
    x <- integer
    return (x, 1)

parseDie :: Parsec' (Int, Int)
parseDie = char 'd' >> integer >>= \x -> return (1, x) 

integer :: Parsec' Int
integer = read <$> many1 digit
