import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Graph (Tree(..))
import Data.List (sortBy)

type ParsedType = ([(Int, Int)], [[Int]])

createUpdateFunction :: Eq a => [(a, a)] -> a -> (a -> Bool) -> a -> Bool
createUpdateFunction [] _ f a = f a
createUpdateFunction ((x,y):xs) a f b = (y /= a || x /= b) && createUpdateFunction xs a f b

checkBook :: Eq a => (a -> (a -> Bool) -> (a -> Bool)) -> (a -> Bool) -> [a] -> Bool
checkBook _ _ [] = True
checkBook g f (x:xs) = f x && checkBook g (g x f) xs

midPoints :: [Int] -> Int
midPoints ns = (head . drop (div (length ns) 2)) ns

stage1 :: ParsedType -> Int
stage1 (rules, bss) = (sum . (fmap midPoints)) bss' where
    bss' = filter (checkBook rulesUpdateFunc (\_ -> True)) bss
    rulesUpdateFunc = createUpdateFunction rules

sortFunc :: (Int -> Int) -> Int -> Int -> Ordering
sortFunc f x y = compare (f y) (f x)

setupSortFunc :: Int -> [(Int, Int)] -> (Int -> Int)
setupSortFunc k [] n = k
setupSortFunc k xs n | n == y = k
                     | otherwise = setupSortFunc (k + 1) ys n where
    (ys, y) = findNextHighest xs

findNextHighest :: [(Int, Int)] -> ([(Int, Int)], Int)
findNextHighest xs | findDangling xs xs == [] = ([], 0) 
                   | otherwise = (removeTrail y xs, y) where
    ((_,y):ys) = findDangling xs xs

findDangling :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findDangling [] = id
findDangling [(x,y)] = removeTrail x
findDangling ((x,y):xs) = removeTrail x . findDangling (removeHead x xs)

removeHead :: Int -> [(Int, Int)] -> [(Int, Int)]
removeHead _ [] = []
removeHead a ((x,y):xs) | a == x = removeHead a xs
                         | otherwise = (x,y) : removeHead a xs

removeTrail :: Int -> [(Int, Int)] -> [(Int, Int)]
removeTrail _ [] = []
removeTrail a ((x,y):xs) | a == y = removeTrail a xs
                         | otherwise = (x,y) : removeTrail a xs

sortByRules :: [(Int, Int)] -> [Int] -> [Int]
sortByRules rules xs = sortBy f xs where
    f = sortFunc (setupSortFunc 0 (filter (\(y,z) -> elem y xs && elem z xs) rules))

stage2 :: ParsedType -> Int
stage2 (rules, bss) = sum ((fmap (midPoints . (sortByRules rules))) bss') where
    bss' = filter (not . (checkBook rulesUpdateFunc (\_ -> True))) bss
    rulesUpdateFunc = createUpdateFunction rules

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (,) <$> some (try parseRule) <* spaces <*> some (parseBook <* spaces) <* eof

parseRule :: Parser (Int, Int)
parseRule = (,) <$> parseNum <* char '|' <*> parseNum <* spaces

parseBook :: Parser [Int]
parseBook = some (parseNum <* many (oneOf ","))

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ['0'..'9']))