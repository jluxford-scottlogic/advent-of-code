import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Set (member, intersection, fromList, Set)

type ParsedType = [String]

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (take h xs, drop h xs)
  where
  h = length xs `div` 2

determineSame :: Ord a => ([a], [a]) -> a
determineSame (a, b) = findSimilar (fromList a) b where

findSimilar :: Ord a => Set a -> [a] -> a
findSimilar set (a:as) | member a set = a
                       | otherwise = findSimilar set as

evaluteValue :: [(Char, Integer)] -> Char -> Integer
evaluteValue [] _ = 0
evaluteValue ((a,b):cs) d | a == d = b
                          | otherwise = evaluteValue cs d
    
values :: [(Char, Integer)]
values = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

stage1 :: [String] -> Integer
stage1 = sum . (fmap ((evaluteValue values) . determineSame . halve))

stage2 :: [String] -> Integer
stage2 [] = 0
stage2 (a:b:c:as) = evaluteValue values (findSimilar (intersection (fromList a) (fromList b)) c) + stage2 as

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = parsedRuckSack

parsedRuckSack :: Parser [String]
parsedRuckSack = some (some (oneOf ['A'..'z']) <* newline) <* eof