import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import Data.List (transpose, elemIndex, sort, nub)

type ParsedType = [[Rock]]

data Rock = Round | Cube | NoRock
    deriving (Show, Eq)

calcWeights :: Int -> Int -> [Rock] -> [Int]
calcWeights _ _ [] = []
calcWeights p v (Round:rs) = v : (calcWeights (p - 1) (v - 1) rs)
calcWeights p v (Cube:rs) = (calcWeights (p - 1) (p - 1) rs)
calcWeights p v (NoRock:rs) = (calcWeights (p - 1) v rs)

useSpace :: [[Rock]] -> [[Int]]
useSpace = fmap (\a -> calcWeights (length a) (length a) a)

stage1 :: [[Rock]] -> Int
stage1 = sum . (fmap sum) . useSpace . transpose

emptyRound :: Rock -> Rock
emptyRound Round = NoRock
emptyRound r = r

remakeCol :: Int -> [Rock] -> [Int] -> [Rock]
remakeCol _ rs [] = fmap emptyRound rs
remakeCol v (Cube:rs) ns = Cube : (remakeCol (v - 1) rs ns)
remakeCol v (r:rs) (n:ns) | n == v = Round : (remakeCol (v - 1) rs ns)
                          | otherwise = NoRock : (remakeCol (v - 1) rs (n:ns))

remakeRocksWithSpace :: [[Rock]] -> [[Int]] -> [[Rock]]
remakeRocksWithSpace rss nss = (fmap (\(rs, ns) -> remakeCol (length rs) rs ns)) (zip rss nss)

tilt :: [[Rock]] -> [[Rock]]
tilt rss = remakeRocksWithSpace rss (useSpace rss)

spinCycle :: [[Rock]] -> [[Rock]]
spinCycle rss = (fmap reverse eastTilt) where
    northTilt = tilt (transpose rss)
    westTilt = tilt (transpose northTilt)
    southTilt = tilt (fmap reverse (transpose westTilt))
    eastTilt = tilt (fmap reverse (transpose (fmap reverse southTilt)))

repSpinCycle :: Int -> [[Rock]] -> [[[Rock]]] -> [[[Rock]]]
repSpinCycle 0 rss rsss = rsss
repSpinCycle n rss rsss | elem rss' rsss = rss':rsss
                        | otherwise = repSpinCycle (n - 1) rss' (rss':rsss) where
    rss' = spinCycle rss

reduceOut :: Int -> [Int] -> Int
reduceOut n (0:x:xs) = mod n (x + 1)
reduceOut n (x:xs) = reduceOut n xs

calcWeight :: Int -> [[Rock]] -> Int
calcWeight _ [] = 0
calcWeight p ([]:rss) = calcWeight (p - 1) rss
calcWeight p ((Round:rs):rss) = p + (calcWeight p (rs:rss))
calcWeight p ((_:rs):rss) = calcWeight p (rs:rss)

stage2 :: [[Rock]] -> Int
stage2 rss = (calcWeight (length rss)) (fullCycles !! finalCycle) where
    finalCycle = reduceOut (1 + (head cycleValues) - 1000000000) cycleValues
    cycleValues = reverse ((fmap ((\(Just a) -> a) . ((flip elemIndex) fullCycles))) fullCycles)
    fullCycles = (repSpinCycle 1000000000 rss [])

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some ((some parseRock) <* spaces) <* eof

parseRock :: Parser Rock
parseRock = Round <$ char 'O'
        <|> Cube <$ char '#'
        <|> NoRock <$ char '.'

