import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Map (Map, foldl, insert, empty, map)

data Memory = Memory Int [Bool] | Mask [Maybe Bool]
    deriving (Show, Eq)

genMask :: [Maybe Bool] -> [Bool] -> [Bool]
genMask [] bs = []
genMask (Nothing:ms) [] = False : genMask ms []
genMask (Just m :ms) [] = m : genMask ms []
genMask (Nothing:ms) (b:bs) = b : genMask ms bs
genMask (Just m :ms) (b:bs) = m : genMask ms bs

runMemory :: [Memory] -> ([Bool] -> [Bool]) -> Map Int [Bool] -> Map Int [Bool]
runMemory [] mask map = map
runMemory (Memory k bs : ms) mask map = runMemory ms mask (insert k (mask bs) map)
runMemory (Mask mask : ms) _ map = runMemory ms (genMask mask) map

decodeBinary :: [Bool] -> Int
decodeBinary = Prelude.foldl f 0 . reverse where
    f :: Int -> Bool -> Int
    f n True = n * 2 + 1
    f n False = n * 2

encodeBinary :: Int -> [Bool]
encodeBinary n | n == 0 = []
               | n == 1 = [True]
               | even n = True : encodeBinary (div n 2)
               | otherwise = False : encodeBinary (div (n + 1) 2)

stage1 :: [Memory] -> Int
stage1 ms = Data.Map.foldl (+) 0 (Data.Map.map decodeBinary (runMemory ms id empty))

inputParser :: Parser [Memory]
inputParser = some (memoryParser <* many space) <* eof

maskParser :: Parser [Maybe Bool]
maskParser = string "mask = " *> (reverse <$> some maybeBoolParser)

memoryParser :: Parser Memory
memoryParser = Memory <$ try (string "mem[") <*> intParser <* string "] = " <*> (reverse . encodeBinary <$> intParser)
           <|> Mask <$> maskParser

maybeBoolParser :: Parser (Maybe Bool)
maybeBoolParser = Nothing <$ char 'X'
              <|> Just <$> boolParser

boolParser :: Parser Bool
boolParser = True <$ char '1' <|> False <$ char '0'

intParser :: Parser Int
intParser = read <$ many space <*> some digit <* many space

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Memory])
parsedValue = parseFromFile inputParser "day14Input.txt"

parsedTest :: IO (Either ParseError [Memory])
parsedTest = parseFromFile inputParser "day14TestInput.txt"