import Text.ParserCombinators.Parsec
import Control.Applicative (some)

bruteForce :: [Int] -> Int
bruteForce ns = minimum ([sum (fmap (\a -> abs (a - x)) ns) | x <- ns])

bruteForceStage2 :: [Int] -> Int
bruteForceStage2 ns = minimum ([sum (fmap (\a -> triag (abs (a - x))) ns) | x <- [minimum ns..maximum ns]]) where
    triag :: Int -> Int
    triag n = ((n + 1) * n) `div` 2

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Int])
parsedValue = parseFromFile intListParser "day7input.txt"

parsedTest :: IO (Either ParseError [Int])
parsedTest = parseFromFile intListParser "day7Testinput.txt"

intListParser :: Parser [Int]
intListParser = some (read <$> some digit <* many space <* many (char ',')) <* eof