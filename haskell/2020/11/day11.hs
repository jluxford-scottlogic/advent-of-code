import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Maybe

stepTillStable :: [[Maybe Bool]] -> [[Maybe Bool]]
stepTillStable seats | steppedSeats == seats = seats 
                     | otherwise = stepTillStable steppedSeats where
    steppedSeats = [[f seats x y | y <- [0..length (head seats) - 1]] | x <- [0..length seats - 1]]
    f :: [[Maybe Bool]] -> Int -> Int -> Maybe Bool
    f ns x y | isNothing (ns !! x !! y) = Nothing
             | sum (concatMap (fmap decode . take (min 3 (y + 2)) . drop (y - 1)) (take (min 3 (x + 2)) (drop (x - 1) ns))) >= 5 = Just False
             | sum (concatMap (fmap decode . take (min 3 (y + 2)) . drop (y - 1)) (take (min 3 (x + 2)) (drop (x - 1) ns))) == 0 = Just True
             | otherwise = ns !! x !! y

decode :: Maybe Bool -> Int
decode Nothing = 0
decode (Just False) = 0
decode (Just True) = 1

stage1 :: [[Maybe Bool]] -> Int
stage1 = sum . fmap (sum . fmap decode) . stepTillStable

inputParser :: Parser [[Maybe Bool]]
inputParser = some (some seatParser <* many space) <* eof

seatParser :: Parser (Maybe Bool)
seatParser = Nothing <$ char '.'
         <|> Just <$> (False <$ char 'L' <|> True <$ char '#')

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [[Maybe Bool]])
parsedValue = parseFromFile inputParser "day11Input.txt"

parsedTest :: IO (Either ParseError [[Maybe Bool]])
parsedTest = parseFromFile inputParser "day11TestInput.txt"