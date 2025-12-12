import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (transpose)

data Tok = Val Integer | Mult | Plus
    deriving Show

type ParsedType = [[Tok]]

maths :: [[Tok]] -> [Integer]
maths [] = []
maths ((_:(Val n):[]):toks) = n : (maths toks)
maths ((Mult:(Val n):(Val m):ns):toks) = maths ((Mult:(Val (n * m)):ns):toks)
maths ((Plus:(Val n):(Val m):ns):toks) = maths ((Plus:(Val (n + m)):ns):toks)

stage1 :: [[Tok]] -> Integer
stage1 = sum . maths . transpose

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

parseLine :: Parser [Tok]
parseLine = some (parseTok <* (many (char ' ')))

parseTok :: Parser Tok
parseTok = Val <$> parseNum
       <|> Mult <$ char '*'
       <|> Plus <$ char '+'

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ['0'..'9']))

data MP = M | P
    deriving Show

type ParsedType2 = ([MP], [[[Int]]])

maths2 :: [MP] -> [[Int]] -> [Int]
maths2 [] [] = []
maths2 (_:ops) ([v]:vss) = v : (maths2 ops vss)
maths2 (P:ops) ((a:b:vs):vss) = maths2 (P:ops) (((a + b):vs):vss)
maths2 (M:ops) ((a:b:vs):vss) = maths2 (M:ops) (((a * b):vs):vss)

calcVal :: [Int] -> Int
calcVal [] = 0
calcVal (0:ns) = calcVal ns
calcVal (n:ns) = n + (10 * (calcVal ns))

calcVals :: [[Int]] -> [Int]
calcVals vss | concat vss == [] = []
             | otherwise = calcVal (reverse (fmap head vss)) : calcVals (fmap tail vss)

stage2 :: ([MP], [[[Int]]]) -> Int
stage2 (ops, vals) = sum (maths2 ops ((fmap calcVals) (transpose vals)))

parsedFile2 :: String -> IO (Either ParseError ParsedType2)
parsedFile2 f = parseFromFile usedParser2 f

parsedTest2 :: IO (Either ParseError ParsedType2)
parsedTest2 = parseFromFile usedParser2 "test.txt"

parsedInput2 :: IO (Either ParseError ParsedType2)
parsedInput2 = parseFromFile usedParser2 "input.txt"

usedParser2 :: Parser ParsedType2
usedParser2 = do
    os <- some parseMP <* space
    is <- some (parseDigitList (snd (unzip os)))
    return (fst (unzip os), is)

parseMP :: Parser (MP, Int)
parseMP = (,) <$> (M <$ char '*' <|> P <$ char '+') <*> countSpaces

countSpaces :: Parser Int
countSpaces = try ((+1) <$> (char ' ' *> countSpaces)) <|> (return 0)

parseDigitList :: [Int] -> Parser [[Int]]
parseDigitList [] = [] <$ space
parseDigitList (x:xs) = (:) <$> parseDigits x <*> parseDigitList xs

parseDigits :: Int -> Parser [Int]
parseDigits 0 = [] <$ space
parseDigits n = (:) <$> parseDigit <*> (parseDigits (n - 1))

parseDigit :: Parser Int
parseDigit = (read . (:[])) <$> (oneOf ['1'..'9']) <|> 0 <$ char ' '