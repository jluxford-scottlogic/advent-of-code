import Text.ParserCombinators.Parsec
import Control.Applicative (some)

data Instr = NoOp | AddX Integer
    deriving (Show, Eq)

type ParsedType = [Instr]

part1 = sumOps 0 1 sumOn

sumOn :: [Integer]
sumOn = [20, 60, 100, 140, 180, 220]

sumOps :: Integer -> Integer -> [Integer] -> [Instr] -> Integer
sumOps _ _ [] _ = 0
sumOps time x (t:ts) [] | t <= time = (t * x)
                        | otherwise = 0
sumOps time x (t:ts) (NoOp:is) | t <= time = (t * x) + (sumOps (time + 1) x ts is)
                               | otherwise = sumOps (time + 1) x (t:ts) is
sumOps time x (t:ts) ((AddX v):is) | t == time = (t * x) + (sumOps (time + 2) (x + v) ts is)
                                   | t < time = (t * x) + (sumOps (time + 2) (x + v) ts is)
                                   | otherwise = sumOps (time + 2) (x + v) (t:ts) is

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseInstr <* newline) <* eof

parseInstr :: Parser Instr
parseInstr = NoOp <$ string "noop"
         <|> AddX <$ string "addx " <*> parseNum

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))