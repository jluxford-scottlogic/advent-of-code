
import Text.ParserCombinators.Parsec
import Control.Applicative (some)

numberOfIncreases :: [Int] -> Int
numberOfIncreases [] = 0
numberOfIncreases [a] = 0
numberOfIncreases (a:b:as) | b > a = 1 + numberOfIncreases (b:as)
                           | otherwise = numberOfIncreases (b:as)

numberOfIncreasesSliding :: [Int] -> Int
numberOfIncreasesSliding [] = 0
numberOfIncreasesSliding [a] = 0
numberOfIncreasesSliding [a,b] = 0
numberOfIncreasesSliding [a,b,c] = 0
numberOfIncreasesSliding (a:b:c:d:as) | d > a = 1 + numberOfIncreasesSliding (b:c:d:as)
                                      | otherwise = numberOfIncreasesSliding (b:c:d:as)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Int])
parsedValue = parseFromFile intListParser "day1Input.txt"

intListParser :: Parser [Int]
intListParser = some (read <$> some digit <* many space)
