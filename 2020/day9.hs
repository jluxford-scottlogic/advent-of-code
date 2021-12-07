import Text.ParserCombinators.Parsec
import Control.Applicative (some)


checkInput :: Int -> [Int] -> [Int] -> Int
checkInput n as [] = 0
checkInput n as (b:bs) | not (or [b == x + y | x <- take n as, y <- take n as]) = b
                       | otherwise = checkInput n (b:as) bs

testInput :: Int -> [Int] -> Int
testInput n as = checkInput n (reverse (take n as)) (drop n as)

findConsecutive :: Int -> [Int] -> Int
findConsecutive n xs = snd $ head filteredResult where
    filteredResult = filter (\(a,b) -> a == n) resultSummed
    resultSummed = fmap (\a -> (sum a, maximum a + minimum a)) allPossible
    allPossible = [take y (drop x xs) | x <- [0..length xs - 1], y <- [1..length xs - x]]

intListParser :: Parser [Int]
intListParser = some (read <$> some digit <* many space)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Int])
parsedValue = parseFromFile intListParser "C:/Dev/advent-of-code/2020/day9Input.txt"

parsedTest :: IO (Either ParseError [Int])
parsedTest = parseFromFile intListParser "C:/Dev/advent-of-code/2020/day9TestInput.txt"

parsedTest2 :: IO (Either ParseError [Int])
parsedTest2 = parseFromFile intListParser "C:/Dev/advent-of-code/2020/day9TestInput2.txt"