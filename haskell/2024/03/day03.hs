import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [Either (Int, Int) Bool]

stage1 :: [Either (Int, Int) Bool] -> Int
stage1 = sum . (fmap multIfJust)

multIfJust :: Either (Int, Int) Bool -> Int
multIfJust (Right x) = 0
multIfJust (Left (a,b)) = a * b

flipOnNothingSum :: Bool -> [Either (Int, Int) Bool] -> Int
flipOnNothingSum b [] = 0
flipOnNothingSum _ ((Right b) : xs) = flipOnNothingSum b xs
flipOnNothingSum b (Left (x, y) : xs) | b = x * y + flipOnNothingSum b xs
                                      | otherwise = flipOnNothingSum b xs

stage2 :: [Either (Int, Int) Bool] -> Int
stage2 = flipOnNothingSum True


runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (:) <$> try parseValid <*> usedParser
           <|> ignoreChar *> usedParser
           <|> [] <$ eof

parseValid :: Parser (Either (Int, Int) Bool)
parseValid = Left <$> ((,) <$ string "mul(" <*> parseNum <* char ',' <*> parseNum <* char ')')
         <|> try ((Right True) <$ string "do()")
         <|> (Right False) <$ string "don't()"

ignoreChar :: Parser Char
ignoreChar = (noneOf "")

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ['0'..'9']))