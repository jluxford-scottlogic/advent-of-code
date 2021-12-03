import Text.ParserCombinators.Parsec
import Control.Applicative (some)


calcBits :: ((Int, Int) -> Int) -> [[Int]] -> [Int]
calcBits _ [] = []
calcBits f bss | head bss == [] = []
               | otherwise = b : css where
    bitColumn = fmap head bss
    css = calcBits f (fmap tail bss)
    b = f (foldl testFunction (0, 0) bitColumn)

removeByBits :: ((Int, Int) -> Int) -> [[Int]] -> [Int]
removeByBits f [] = []
removeByBits f [bs] = bs
removeByBits f bss = b : css where
    bitColumn = fmap head bss
    css = removeByBits f (fmap tail (filter (\(a:as) -> a == b) bss))
    b = f (foldl testFunction (0, 0) bitColumn)

testFunction :: (Int, Int) -> Int -> (Int, Int)
testFunction (a, b) x | x == 0 = (a + 1, b)
                      | otherwise = (a, b + 1)

gamma :: (Int, Int) -> Int
gamma (a, b) | a > b = 0
             | otherwise = 1

epsilon :: (Int, Int) -> Int
epsilon (a, b) | a <= b = 0
             | otherwise = 1

deBit :: [Int] -> Int
deBit = foldl (\ a b -> a * 2 + b) 0

gammaTimesEpsilon :: [[Int]] -> Int
gammaTimesEpsilon bss = gam * eps where
    gam = deBit (calcBits gamma bss)
    eps = deBit (calcBits epsilon bss)

oxygenTimesCO2 :: [[Int]] -> Int
oxygenTimesCO2 bss = oxy * co2 where
    oxy = deBit (removeByBits gamma bss)
    co2 = deBit (removeByBits epsilon bss)

day4Parser :: Parser [[Int]]
day4Parser = some (some bitParser <* many space) <* eof

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [[Int]])
parsedValue = parseFromFile day4Parser "C:/Dev/advent-of-code/2021/day3input.txt"

parsedTest :: IO (Either ParseError [[Int]])
parsedTest = parseFromFile day4Parser "C:/Dev/advent-of-code/2021/day3Testinput.txt"

bitParser :: Parser Int
bitParser = read . (:[]) <$> digit