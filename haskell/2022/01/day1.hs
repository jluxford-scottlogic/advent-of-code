import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (sort)

findMax :: [[Integer]] -> Integer
findMax = maximum . (fmap sum)

findTopThree :: [[Integer]] -> Integer
findTopThree = sum . (take 3) . reverse . sort  . (fmap sum)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedNumLine :: String -> IO (Either ParseError [[Integer]])
parsedNumLine f = parseFromFile everyoneParser f

parsedTest :: IO (Either ParseError [[Integer]])
parsedTest = parseFromFile everyoneParser "test.txt"

parsedInput :: IO (Either ParseError [[Integer]])
parsedInput = parseFromFile everyoneParser "input.txt"

everyoneParser :: Parser [[Integer]]
everyoneParser = some (personParser <* newline) <* eof

personParser :: Parser [Integer]
personParser = some (doubleParser <* newline)

doubleParser :: Parser Integer
doubleParser = read <$> (some (oneOf ['0'..'9']))