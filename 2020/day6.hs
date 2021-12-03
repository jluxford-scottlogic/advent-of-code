import Text.ParserCombinators.Parsec
import Control.Applicative (some)

countAll :: ([String] -> Int) -> [[String]] -> Int
countAll f = sum . fmap f

countGroupAny :: [String] -> Int
countGroupAny = length . condense . concat where
    condense :: Eq a => [a] -> [a]
    condense [] = []
    condense (a:as) = a : condense (filter (/= a) as)

countGroupAll :: [String] -> Int
countGroupAll = length . condense where
    condense :: Eq a => [[a]] -> [a]
    condense [] = []
    condense ([]:ass) = []
    condense ((a:as):ass) = f a ass ++ condense (as:ass)
    f :: Eq a => a -> [[a]] -> [a]
    f a [] = [a]
    f a (as:ass) | a `elem` as = f a ass
                 | otherwise = []

inputParser :: Parser [[String]]
inputParser = some (some lineParse <* char '\n') <* eof

lineParse :: Parser String
lineParse = some (oneOf ['a'..'z']) <* char '\n'

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [[String]])
parsedValue = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day6Input.txt"

parsedTest :: IO (Either ParseError [[String]])
parsedTest = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day6TestInput.txt"