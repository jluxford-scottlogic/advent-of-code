import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)

type ParsedType = [(Integer, [Integer])]

checkThrough :: [(Integer -> Integer -> Integer)] -> (Integer, [Integer]) -> Bool
checkThrough fs (x, [y]) = x == y
checkThrough fs (x, y:z:ys) = or [checkThrough fs (x, f y z : ys) | f <- fs]

stage1 :: ParsedType -> Integer
stage1 = sum . fmap fst . filter (checkThrough [(+), (*)])

smoosh :: Integer -> Integer -> Integer
smoosh x y = read (show x ++ show y)

stage2 :: ParsedType -> Integer
stage2 = sum . fmap fst . filter (checkThrough [(+), (*), smoosh])

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

parseLine :: Parser (Integer, [Integer])
parseLine = (,) <$> parseNum <* string ":" <*> some (try (space *> parseNum))

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ['0'..'9']))