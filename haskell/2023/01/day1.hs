import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)

type ParsedType = [[Int]]

digititseAndRev :: [Int] -> Int
digititseAndRev (x:xs) = x * 10 + y where
    (y:ys) = reverse (x:xs)

stage1 :: ParsedType -> Int
stage1 = sum . (fmap digititseAndRev)

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

parseLine :: Parser [Int]
parseLine = some (parseAlpha *> parseNum <* parseAlpha)

parseAlpha :: Parser String
parseAlpha = many (try (oneOf ['a'..'z']))

parseNum :: Parser Int
parseNum = (read . (:[])) <$> (oneOf (['0'..'9']))

undigit :: [Either Int Char] -> [Int]
undigit [] = []
undigit (Left n : ns) = n : (undigit ns)
undigit (Right 'o' : zs@(Right 'n' : Right 'e' : _)) = 1 : (undigit zs)
undigit (Right 't' : zs@(Right 'w' : Right 'o' : _)) = 2 : (undigit zs)
undigit (Right 't' : zs@(Right 'h' : Right 'r' : Right 'e' : Right 'e' : _)) = 3 : (undigit zs)
undigit (Right 'f' : zs@(Right 'o' : Right 'u' : Right 'r' : _)) = 4 : (undigit zs)
undigit (Right 'f' : zs@(Right 'i' : Right 'v' : Right 'e' : _)) = 5 : (undigit zs)
undigit (Right 's' : zs@(Right 'i' : Right 'x' : _)) = 6 : (undigit zs)
undigit (Right 's' : zs@(Right 'e' : Right 'v' : Right 'e' : Right 'n' : _)) = 7 : (undigit zs)
undigit (Right 'e' : zs@(Right 'i' : Right 'g' : Right 'h' : Right 't' : _)) = 8 : (undigit zs)
undigit (Right 'n' : zs@(Right 'i' : Right 'n' : Right 'e' : _)) = 9 : (undigit zs)
undigit (Right _ : ns) = undigit ns

stage2 :: [[Either Int Char]] -> Int
stage2 = stage1 . (fmap (undigit))

parsedTestS2 :: IO (Either ParseError [[Either Int Char]])
parsedTestS2 = parseFromFile usedParser2 "test.txt"

parsedTestS22 :: IO (Either ParseError [[Either Int Char]])
parsedTestS22 = parseFromFile usedParser2 "test2.txt"

parsedInputS2 :: IO (Either ParseError [[Either Int Char]])
parsedInputS2 = parseFromFile usedParser2 "input.txt"

usedParser2 :: Parser [[Either Int Char]]
usedParser2 = many (parseLine2 <* (many space)) <* eof

parseLine2 :: Parser [Either Int Char]
parseLine2 = some (parseNum2)

parseNum2 :: Parser (Either Int Char)
parseNum2 = (Left . read . (:[])) <$> (oneOf (['0'..'9']))
       <|> Right <$> (oneOf ['a'..'z'])