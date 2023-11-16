import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (sort)

type ParsedType = String

-- stage1 :: String -> Integer
-- stage1 (a:b:c:d:as) | anyMatch a b c d = 1 + stage1 (b:c:d:as)
--                     | otherwise = 4

-- anyMatch :: Char -> Char -> Char -> Char -> Bool
-- anyMatch a b c d = a == b || a == c || a == d || b == c || b == d || c == d

stage2 :: Int -> String -> Int
stage2 n as | anyMatch (take n as) = 1 + stage2 n (drop 1 as)
            | otherwise = n

anyMatch :: [Char] -> Bool
anyMatch as = f (sort as) where
    f :: [Char] -> Bool
    f (a:[]) = False
    f (a:b:as) | a == b = True
               | otherwise = f (b:as)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (oneOf ['A'..'z']) <* eof