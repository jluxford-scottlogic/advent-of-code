import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (sort)

type ParsedType = [(Int, Int)]

diffInput :: ([Int], [Int]) -> Int
diffInput ([], []) = 0
diffInput (x:xs, y:ys) = abs (x - y) + (diffInput (xs, ys))

sortInput :: ([Int], [Int]) -> ([Int], [Int])
sortInput (xs, ys) = (sort xs, sort ys)

flipInput :: [(Int, Int)] -> ([Int], [Int])
flipInput [] = ([], [])
flipInput ((x,y):zs) = (x:xs, y:ys) where
    (xs, ys) = flipInput zs

stage1 :: [(Int, Int)] -> Int
stage1 = diffInput . sortInput . flipInput

simInput :: ([Int], [Int]) -> Int
simInput ([], _) = 0
simInput (_, []) = 0
simInput (x:xs, y:ys) | x > y = simInput (x:xs, ys)
                      | y > x = simInput (xs, y:ys)
                      | otherwise = (simCheck x (y:ys)) + (simInput (xs, y:ys))

simCheck :: Int -> [Int] -> Int
simCheck x [] = 0
simCheck x (y:ys) | x == y = x + simCheck x ys
                  | otherwise = 0

stage2 :: [(Int, Int)] -> Int
stage2 = simInput . sortInput . flipInput

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseLine) <* eof

parseLine :: Parser (Int, Int)
parseLine = (,) <$> parseNum <* spaces <*> parseNum <* spaces

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))