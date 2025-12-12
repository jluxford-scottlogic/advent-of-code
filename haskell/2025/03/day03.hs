import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [[Int]]

joltageLineFold :: Int -> (Int, Int) -> (Int, Int)
joltageLineFold m (0, 0) = (0, m)
joltageLineFold m (0, n) = (m, n)
joltageLineFold new (upper, lower) | new >= upper = (new, max upper lower)
                                   | otherwise = (upper, lower)

combineJoltage :: (Int, Int) -> Int
combineJoltage (ten, unit) = ten * 10 + unit

stage1 :: [[Int]] -> Int
stage1 = sum . (map (combineJoltage . foldr joltageLineFold (0,0)))

joltageLineFoldArb :: [Int] -> [Int]
joltageLineFoldArb [x] = []
joltageLineFoldArb (y:x:xs) | y >= x = y:(joltageLineFoldArb (x:xs))
                            | otherwise = (x:xs)

combineJoltageArb :: [Int] -> Int
combineJoltageArb [unit] = unit
combineJoltageArb (ten:(unit:units)) = combineJoltageArb ((ten * 10 + unit):units)

calcJoltage :: Int -> [Int] -> Int
calcJoltage len ns = combineJoltageArb (foldr (\b -> \as -> joltageLineFoldArb (b:as)) back front) where
    lenX = length ns
    front = take (lenX - len) ns
    back = drop (lenX - len) ns

stage2 :: [[Int]] -> Int
stage2 = sum . (map (calcJoltage 12))

stage1Using2 :: [[Int]] -> Int
stage1Using2 = sum . (map (calcJoltage 2))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (some parseNum <* spaces) <* eof

parseNum :: Parser Int
parseNum = read <$> ((:[]) <$> oneOf ['0'..'9'])