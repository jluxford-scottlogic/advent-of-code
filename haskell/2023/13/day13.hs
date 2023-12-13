import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import Data.List (transpose)

type ParsedType = ([[[Bool]]])

refEq :: Eq a => [a] -> [a] -> Bool
refEq [] __ = True
refEq __ [] = True
refEq (x:xs) (y:ys) = x == y && (refEq xs ys)

reflection :: ([a] -> [a] -> Bool) -> [a] -> [a] -> Int
reflection f [] ys = 0
reflection f (x:xs) [] = reflection f xs [x]
reflection f (x:xs) ys | f (x:xs) ys = length ys
                       | otherwise = reflection f xs (x:ys)

evalChunk :: ([[Bool]] -> [[Bool]] -> Int) -> [[Bool]] -> Int
evalChunk f bss = rows + cols where
    rows = 100 * (f bss [])
    cols = f (transpose bss) []

stage1 :: ParsedType -> Int
stage1 = sum . (fmap (evalChunk (reflection refEq)))

oneOff :: Eq a => [a] -> [a] -> Bool
oneOff [] [] = False
oneOff (x:xs) (y:ys) | x == y = oneOff xs ys
                     | otherwise = refEq xs ys

oneOffOnce :: Eq a => [[a]] -> [[a]] -> Bool
oneOffOnce [] __ = False
oneOffOnce __ [] = False
oneOffOnce (xs:xss) (ys:yss) | xs == ys = oneOffOnce xss yss
                             | oneOff xs ys = refEq xss yss
                             | otherwise = False

stage2 :: ParsedType -> Int
stage2 = sum . (fmap (evalChunk (reflection oneOffOnce)))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseChunk <* string "\r\n") <* eof

parseChunk :: Parser [[Bool]]
parseChunk = some (parseLine <* string "\r\n")

parseLine :: Parser [Bool]
parseLine = some (parseCell)

parseCell :: Parser Bool
parseCell = True <$ string "#" <|> False <$ string "."