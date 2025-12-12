import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (sort)

type ParsedType = ([(Int, Int)], [Int])

testAgainstRange :: [(Int, Int)] -> [Int] -> Int
testAgainstRange _ [] = 0
testAgainstRange [] _ = 0
testAgainstRange ((ll, ul):ls) (f:fs) | ll <= f && ul >= f = 1 + testAgainstRange ((ll,ul):ls) fs
                                      | f < ll = testAgainstRange ((ll, ul):ls) fs
                                      | ul < f = testAgainstRange ls (f:fs)

stage1 :: ([(Int, Int)], [Int]) -> Int
stage1 (ls, fs) = testAgainstRange (sort ls) (sort fs)

combineRanges :: [(Int, Int)] -> [(Int, Int)]
combineRanges [(l,u)] = [(l,u)]
combineRanges ((l,u):(a,b):ls) | a <= u = combineRanges ((l, max u b):ls)
                               | otherwise = (l,u) : (combineRanges ((a,b):ls))

sumRanges :: [(Int, Int)] -> Int
sumRanges [] = 0
sumRanges ((l,u):ls) = u - l + 1 + (sumRanges ls)

stage2 :: ([(Int, Int)], [Int]) -> Int
stage2 (ls,_) = sumRanges . combineRanges . sort $ ls

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (,) <$> some (try parseRange) <*> some (parseNum <* spaces) <* eof

parseRange :: Parser (Int, Int)
parseRange = (,) <$> parseNum <* char '-' <*> parseNum <* spaces

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ['0'..'9']))