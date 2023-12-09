import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)

type ParsedType = [[Int]]

calcDiffsTillStable :: [[Int]] -> [[Int]]
calcDiffsTillStable (xs:xss) | allSame xs = (xs:xss)
                             | otherwise = calcDiffsTillStable ((calcDiffs xs): xs : xss)

calcDiffs :: [Int] -> [Int]
calcDiffs [] = []
calcDiffs [a] = []
calcDiffs (a:b:as) = (b - a) : (calcDiffs (b:as))

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame [a] = True
allSame (a:b:as) = a == b && (allSame (b:as))

nextValue :: [Int] -> Int
nextValue [x] = x
nextValue (x:y:xs) = nextValue ((y - x):xs)

stage1 :: ParsedType -> Int
stage1 = sum . (fmap (nextValue . (fmap head) . calcDiffsTillStable . (:[]) . reverse))

stage2 :: ParsedType -> Int
stage2 = sum . (fmap (nextValue . (fmap head) . calcDiffsTillStable . (:[])))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseLine <* spaces) <* eof

parseLine :: Parser [Int]
parseLine = some (parseNum <* (many (char ' ')))

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))