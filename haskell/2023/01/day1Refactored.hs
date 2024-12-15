import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [[Either Int Char]]

digititseAndRev :: [Int] -> Int
digititseAndRev (x:xs) = x * 10 + y where
    (y:ys) = reverse (x:xs)

grabDigits :: [Either Int Char] -> [Int]
grabDigits [] = []
grabDigits (Left n : ns) = n : (grabDigits ns)
grabDigits (Right _ : ns) = grabDigits ns

stage1 :: ParsedType -> Int
stage1 = sum . (fmap digititseAndRev) . (fmap grabDigits)

undigit :: [Either Int Char] -> [Int]
undigit [] = []
undigit (Left n : ns) = n : (undigit ns)
undigit (Right 'o' : Right 'n' : zs@(Right 'e' : _))                         = 1 : (undigit zs)
undigit (Right 't' : Right 'w' : zs@(Right 'o' : _))                         = 2 : (undigit zs)
undigit (Right 't' : Right 'h' : Right 'r' : Right 'e' : zs@(Right 'e' : _)) = 3 : (undigit zs)
undigit (Right 'f' : Right 'o' : Right 'u' : Right 'r' : ns)                 = 4 : (undigit ns)
undigit (Right 'f' : Right 'i' : Right 'v' : zs@(Right 'e' : _))             = 5 : (undigit zs)
undigit (Right 's' : Right 'i' : Right 'x' : ns)                             = 6 : (undigit ns)
undigit (Right 's' : Right 'e' : Right 'v' : Right 'e' : zs@(Right 'n' : _)) = 7 : (undigit zs)
undigit (Right 'e' : Right 'i' : Right 'g' : Right 'h' : zs@(Right 't' : _)) = 8 : (undigit zs)
undigit (Right 'n' : Right 'i' : Right 'n' : zs@(Right 'e' : _))             = 9 : (undigit zs)
undigit (Right _ : ns) = undigit ns

stage2 :: [[Either Int Char]] -> Int
stage2 = sum . (fmap digititseAndRev) . (fmap undigit)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedTest2 :: IO (Either ParseError ParsedType)
parsedTest2 = parseFromFile usedParser "test2.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = many (parseLine <* (many space)) <* eof

parseLine :: Parser [Either Int Char]
parseLine = some (parseNum)

parseNum :: Parser (Either Int Char)
parseNum = (Left . read . (:[])) <$> (oneOf (['0'..'9']))
       <|> Right <$> (oneOf ['a'..'z'])