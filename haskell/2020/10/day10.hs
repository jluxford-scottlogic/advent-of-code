import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (sort)

joltDifferences :: [Int] -> (Int, Int, Int)
joltDifferences = snd . foldl f (0, (0,0,1)) . sort where
    f :: (Int, (Int, Int, Int)) -> Int -> (Int, (Int, Int, Int))
    f (x, (one, two, three)) y | y - x == 1 = (y, (one + 1, two, three))
                               | y - x == 2 = (y, (one, two + 1, three))
                               | y - x == 3 = (y, (one, two, three + 1))
                               | otherwise = undefined

stage1 :: [Int] -> Int
stage1 = (\(a,_,c) -> a*c) . joltDifferences

countRoutes :: [Int] -> Int 
countRoutes = head . f [1] . sort where
    f :: [Int] -> [Int] -> [Int]
    f js [] = js
    f js (a:as) | a == length js = f (sum (take 3 js) : js) as
                | otherwise = f (0:js) (a:as)

intListParser :: Parser [Int]
intListParser = some (read <$> some digit <* many space)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Int])
parsedValue = parseFromFile intListParser "day10Input.txt"

parsedTest :: IO (Either ParseError [Int])
parsedTest = parseFromFile intListParser "day10TestInput.txt"

parsedTest2 :: IO (Either ParseError [Int])
parsedTest2 = parseFromFile intListParser "day10TestInput2.txt"