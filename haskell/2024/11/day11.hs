import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))

type ParsedType = [Integer]

type Map = Map.Map (Integer, Integer) Integer

lookupValue :: Map -> Integer -> Integer -> (Map, Integer)
lookupValue map 0 value = (map, 1)
lookupValue map blinks value | Map.member (blinks, value) map = (map, map ! (blinks, value))
                             | otherwise = (Map.insert (blinks, value) valSum map', valSum) where
    vals' = blink value
    (map', valSum) = calcRows map (blinks - 1) vals'

blink :: Integer -> [Integer]
blink 0 = [1]
blink x | mod lenX 2 == 0 = [xLeft, xRight]
        | otherwise = [x * 2024] where
            lenX = length (show x)
            xLeft = div x (10 ^ (div lenX 2))
            xRight = mod x (10 ^ (div lenX 2))

calcRows :: Map -> Integer -> [Integer] -> (Map, Integer)
calcRows map blinks [] = (map, 0)
calcRows map blinks (x:xs) = fmap ((+) val) (calcRows map' blinks xs) where
    (map', val) = lookupValue map blinks x

stage1 :: [Integer] -> Integer
stage1 =snd . calcRows Map.empty 25

stage2 :: [Integer] -> Integer
stage2 =snd . calcRows Map.empty 75

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseNum <* (many space)) <* eof

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ['0'..'9']))