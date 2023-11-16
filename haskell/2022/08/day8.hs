import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (transpose)

type ParsedType = [[Integer]]

--             (Height,  Visible)
type Trees1 = [[(Integer, Bool)]]

setBase1 :: ParsedType -> Trees1
setBase1 = fmap (fmap (\a -> (a, False)))

--              (Height,  Scenic Score)
type Trees2 = [[(Integer, Integer)]]

setBase2 :: ParsedType -> Trees2
setBase2 = fmap (fmap (\a -> (a, 1)))

checkVisibleMax :: Integer -> [(Integer, Bool)] -> [(Integer, Bool)]
checkVisibleMax _ [] = []
checkVisibleMax n ((h, b):hs) | h > n = (h, True) : (checkVisibleMax h hs)
                              | otherwise = (h, b) : (checkVisibleMax n hs)

checkVisible :: [(Integer, Bool)] -> [(Integer, Bool)]
checkVisible [] = []
checkVisible ((h, b):hs) = (h, True) : checkVisibleMax h hs

doAllDirections :: ([(Integer, a)] -> [(Integer, a)]) -> [[(Integer, a)]] -> [[(Integer, a)]]
doAllDirections f ms = dir4 where
    dir1 = fmap f ms
    dir2 = fmap (f . reverse) dir1
    dir3 = fmap f (transpose dir2)
    dir4 = fmap (f . reverse) dir3

sumAll :: [[(Integer, Bool)]] -> Integer
sumAll = sum . (fmap (foldr sumIfTrue 0)) where
    sumIfTrue :: (a, Bool) -> Integer -> Integer
    sumIfTrue (_, True) n = n + 1
    sumIfTrue (_, False) n = n

stage1 :: [[Integer]] -> Integer
stage1 = sumAll . (doAllDirections checkVisible) . setBase1

distanceIntoList :: [Integer] -> Integer -> Integer
distanceIntoList [] _ = 0
distanceIntoList (n : ns) h | h > n = 1 + distanceIntoList ns h
                            | otherwise = 1

scenicMultList :: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
scenicMultList _ [] = []
scenicMultList ns ((h, s) : hs) = (h, s * (distanceIntoList ns h)) : scenicMultList (h:ns) hs

scenicMult :: [(Integer, Integer)] -> [(Integer, Integer)]
scenicMult ((h, s): hs) = (h, 0) : (scenicMultList [h] hs)

maxAll :: [[(Integer, Integer)]] -> Integer
maxAll = maximum . (fmap maximum) . (fmap (fmap snd))

stage2 :: [[Integer]] -> Integer
stage2 = maxAll . (doAllDirections scenicMult) . setBase2

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some ((some parseDigit) <* (many newline)) <* eof

parseDigit :: Parser Integer
parseDigit = read . (:[]) <$> (oneOf ['0'..'9'])