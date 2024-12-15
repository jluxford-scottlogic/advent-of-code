import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [(Signal,Signal)]

data Signal = SubSignal Signal Signal | Value Int Signal | End
    deriving (Show, Eq)

compareSignals :: (Signal, Signal) -> Maybe Bool
compareSignals (Value l sl, Value r sr) | l < r = Just True
                                        | l > r = Just False
                                        | otherwise = compareSignals (sl, sr)
compareSignals (SubSignal l sl, SubSignal r sr) | compareSignals (l, r) == Nothing = compareSignals (sl, sr)
                                                | otherwise = compareSignals (l, r)
compareSignals (End, End) = Nothing
compareSignals (End, _) = Just True
compareSignals (_, End) = Just False
compareSignals (sl, Value r sr) = compareSignals (sl, (SubSignal (Value r End) sr))
compareSignals (Value l sl, sr) = compareSignals ((SubSignal (Value l End) sl), sr)

sumTrueIndices :: Int -> [Maybe Bool] -> Int
sumTrueIndices _ [] = 0
sumTrueIndices n (Just True : bs) = n + (sumTrueIndices (n + 1) bs)
sumTrueIndices n (Just False : bs) = (sumTrueIndices (n + 1) bs)
sumTrueIndices n (Nothing : bs) = (sumTrueIndices (n + 1) bs)

stage1 :: ParsedType -> Int
stage1 = (sumTrueIndices 1) . (fmap compareSignals)

mergeSort :: [Signal] -> [Signal]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = rebuild (mergeSort as) (mergeSort bs) where
    (as, bs) = split xs


split :: [a] -> ([a], [a])
split [] = ([], [])
split [a] = ([], [a])
split (a:b:cs) = (a:as, b:bs) where
    (as, bs) = split cs

rebuild :: [Signal] -> [Signal] -> [Signal]
rebuild [] bs = bs
rebuild as [] = as
rebuild (a:as) (b:bs) | compareSignals (a, b) == Just True = a : (rebuild as (b:bs))
                      | otherwise = b : (rebuild (a:as) bs)

findX :: Eq a => a -> [a] -> Int
findX a [] = undefined
findX a (b:bs) | a == b = 1
               | otherwise = 1 + (findX a bs)

packet2 :: Signal
packet2 = SubSignal (Value 2 End) End

packet6 :: Signal
packet6 = SubSignal (Value 6 End) End

stage2 :: ParsedType -> Int
stage2 xs = (findX packet2 ss) * (findX packet6 ss) where
    f :: (Signal, Signal) -> [Signal] -> [Signal]
    f (a, b) as = a:b:as
    ss = mergeSort (foldr f [packet2, packet6] xs)

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
usedParser = some (parseSignalPair) <* eof

parseSignalPair :: Parser (Signal,Signal)
parseSignalPair = (,) <$> (string "[" *> parseSignal) <* many space <*> (string "[" *> parseSignal) <* many space

parseSignal :: Parser Signal
parseSignal = SubSignal <$ string "[" <*> parseSignal <* many (oneOf ",") <*> parseSignal
          <|> Value <$> parseNum <* many (oneOf ",") <*> parseSignal
          <|> End <$ string "]"

parseNum :: Parser Int
parseNum = (read) <$> (some (oneOf (['0'..'9'])))