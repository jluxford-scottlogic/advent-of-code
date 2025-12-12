import Text.ParserCombinators.Parsec
import Control.Applicative (some)

data LR a = L a | R a
    deriving Show

type ParsedType = [LR Integer]

foldStage1 :: (Integer, Integer) -> (LR Integer) -> (Integer, Integer)
foldStage1 (place, zeros) (L n) | (mod (place - n) 100) == 0 = (0, zeros + 1)
                                | otherwise = (mod (place - n) 100, zeros)
foldStage1 (place, zeros) (R n) | (mod (place + n) 100) == 0 = (0, zeros + 1)
                                | otherwise = (mod (place + n) 100, zeros)

stage1 :: [LR Integer] -> Integer
stage1 = snd . (foldl foldStage1 (50,0))

placeZeroAdjustment :: Integer -> Integer
placeZeroAdjustment 0 = 1
placeZeroAdjustment _ = 0

foldStage2 :: (Integer, Integer) -> (LR Integer) -> (Integer, Integer)
foldStage2 (place, zeros) (L n) | (place - n) <= 0 = (mod (place - n) 100, zeros - (div (place - n) 100) - (placeZeroAdjustment place) + (placeZeroAdjustment (mod (place - n) 100)))
                                | otherwise = (place - n, zeros)
foldStage2 (place, zeros) (R n) | (place + n) >= 100 = (mod (place + n) 100, zeros + (div (place + n) 100))
                                | otherwise = (place + n, zeros)

stage2:: [LR Integer] -> Integer
stage2 = snd . (foldl foldStage2 (50,0))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedTestPartial :: IO (Either ParseError ParsedType)
parsedTestPartial = parseFromFile usedParser "testPartial.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some ((parseLR parseNum) <* spaces) <* eof

parseLR :: Parser a -> Parser (LR a)
parseLR p = L <$> (char 'L' *> p)
        <|> R <$> (char 'R' *> p)

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ['0'..'9']))