import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [((Integer, Integer), (Integer, Integer))]

stage1 :: ParsedType -> Integer
stage1 = sum . (fmap isContained)

isContained :: ((Integer, Integer), (Integer, Integer)) -> Integer
isContained ((a,b),(c,d)) | (a <= c && b >= d) || (c <= a && d >= b) = 1
                          | otherwise = 0

stage2 ::ParsedType -> Integer
stage2 = sum . (fmap isOverlap)

isOverlap :: ((Integer, Integer), (Integer, Integer)) -> Integer
isOverlap ((a,b),(c,d)) | (b < c) || (d < a) = 0
                        | otherwise = 1

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseLine <* (many newline)) <* eof

parseLine :: Parser ((Integer, Integer), (Integer, Integer))
parseLine = (,) <$> parseRange <* string "," <*> parseRange

parseRange :: Parser (Integer, Integer)
parseRange = (,) <$> parseNum <* string "-" <*> parseNum

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ['0'..'9']))