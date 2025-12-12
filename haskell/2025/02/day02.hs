import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (nub)

type ParsedType = [(Int, Int)]

intSplit :: Int -> (Int, Int)
intSplit n = (read (take (div stringLen 2) stringValue), read (drop (div stringLen 2) stringValue)) where
    stringValue = show n
    stringLen = length stringValue

findInvalid :: (Int, Int) -> [Int]
findInvalid (0,0) = []
findInvalid (lower, upper) = [lowerStart | lowerStart == upperStart && (lowerEnd <= lowerStart && upperEnd >= upperStart)] ++ [upperStart | upperStart <= upperEnd && lowerStart /= upperStart] ++ [lowerStart | lowerStart >= lowerEnd && lowerStart /= upperStart] ++ [(lowerStart + 1)..(upperStart - 1)] where
    (lowerStart, lowerEnd) = intSplit lower
    (upperStart, upperEnd) = intSplit upper

liftTen :: Int -> Int
liftTen 0 = 1
liftTen n = 10 * (liftTen (n - 1))

logTenBase :: Int -> Int
logTenBase = length . show

isEvenSplit :: Int -> Bool
isEvenSplit = (== 0) . (\x -> mod x 2) . logTenBase

adjustToRange :: (Int, Int) -> (Int, Int)
adjustToRange (lower, upper) | isEvenSplit lower && isEvenSplit upper = (lower, upper)
                             | isEvenSplit upper = (liftTen (logTenBase upper - 1), upper)
                             | isEvenSplit lower = (lower, liftTen (logTenBase lower) - 1)
                             | otherwise = (0,0)

mirrorNumber :: Int -> Int
mirrorNumber n = n + (n * (liftTen (logTenBase n)))

stage1 :: [(Int, Int)] -> Int
stage1 = sum . (map mirrorNumber) . (concatMap findInvalid) . (map adjustToRange)

concatXSplit :: Int -> String -> [Int]
concatXSplit len [] = []
concatXSplit len str = read (take len str) : (concatXSplit len (drop len str))

intXSplit :: Int -> Int -> [Int]
intXSplit x n = concatXSplit (div stringLen x) stringValue where
    stringValue = show n
    stringLen = length stringValue

addVal :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int] -> [Int]
addVal comp start [] ys = ys
addVal comp start (x:xs) ys | start == x = addVal comp start xs ys
                            | comp start x = start:ys
                            | otherwise = ys

findXInvalid :: Int -> (Int, Int) -> [Int]
findXInvalid x (0,0) = []
findXInvalid x (lower, upper) | length lowers > length uppers = []
                              | otherwise = addVal (<) upperStart uppers . addVal (>) lowerStart lowers $ [(lowerStart + 1)..(upperStart - 1)] where
    (lowerStart:lowers) = intXSplit x lower
    (upperStart:uppers) = intXSplit x upper

isXSplit :: Int -> Int -> Bool
isXSplit x = (== 0) . (\n -> mod n x) . logTenBase

adjustToXRange :: Int -> (Int, Int) -> (Int, Int)
adjustToXRange x (lower, upper) | isXSplit x lower && isXSplit x upper = (lower, upper)
                                | isXSplit x upper = (liftTen (logTenBase upper - 1), upper)
                                | isXSplit x lower = (lower, liftTen (logTenBase lower) - 1)
                                | otherwise = (0,0)

liftX :: Int -> Int -> Int
liftX x 0 = 1
liftX x n = x * (liftX x (n - 1))

repeatNumber :: Int -> Int -> Int
repeatNumber repeats n = sum . (map (*n)) . (map (liftX (liftTen (logTenBase n)))) $ [0..(repeats - 1)]

-- stage2 :: [(Int, Int)] -> [Int]
-- stage2 = nub . concat . ((map (\(x, y) -> (map ($ (x,y))) (map (\z -> (map (repeatNumber x)) . findXInvalid x . adjustToXRange x) [1..logTenBase x]))))

check :: String -> String -> String -> Bool
check _ [] [] = True
check _ _ [] = False
check base [] ss = check base base ss
check base (n:ns) (s:ss) | n == s = check base ns ss
                         | otherwise = False

checkRepeat :: [Int] -> String -> Bool
checkRepeat [] _ = False
checkRepeat (i:is) s = check (take i s) "" s || checkRepeat is s

stage2BruteForce :: [(Int, Int)] -> Int
stage2BruteForce [] = 0
stage2BruteForce ((x,y):xs) = sum [ z | z <- [x..y], checkRepeat [1..(length (show z) - 1)] (show z) ] + stage2BruteForce xs

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (:) <$> parsePair <*> ([] <$ eof <|> char ',' *> usedParser)

parsePair :: Parser (Int, Int)
parsePair = (,) <$> parseNum <* char '-' <*> parseNum

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ['0'..'9']))