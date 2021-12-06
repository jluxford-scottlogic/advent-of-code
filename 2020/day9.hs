import Text.ParserCombinators.Parsec
import Control.Applicative (some)


checkInput :: Int -> [Int] -> [Int] -> Int
checkInput n as [] = 0
checkInput n as (b:bs) | not (or [b == x + y | x <- take n as, y <- take n as]) = b
                       | otherwise = checkInput n (b:as) bs

findFailure :: Int -> [Int] -> [Int] -> [Bool]
findFailure n as [] = []
findFailure n as (b:bs) = not (or [b == x + y | x <- take n as, y <- take n as]) : findFailure n (b:as) bs

testInput :: Int -> [Int] -> Int
testInput n as = checkInput n (reverse (take n as)) (drop n as)

testFailure :: Int -> [Int] -> [Bool]
testFailure n as = [False | n <- [1..25]] ++ findFailure n (reverse (take n as)) (drop n as)

actOnFailure :: Int -> [Int] -> Int
actOnFailure n as = maximum list * minimum list where
    failureSet = testFailure n as
    list = g (f as failureSet)
    f :: [Int] -> [Bool] -> (Int, [Int])
    f [] [] = (0,[])
    f (a:as) (b:bs) | b = (a, [])
                    | otherwise = fmap (a:) (f as bs)
    g :: (Int, [Int]) -> [Int]
    g (sumer, ys) = snd $ head $ filter fst [(sum (take x (drop y ys)) == sumer, take x (drop y ys)) | x <- [1..length ys], y <- [0..length ys - x]]

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