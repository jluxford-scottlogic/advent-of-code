import Text.ParserCombinators.Parsec
import Control.Applicative (some)



dayXParser :: Parser ()
dayXParser = eof

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError ())
parsedValue = parseFromFile dayXParser "day9input.txt"

parsedTest :: IO (Either ParseError ())
parsedTest = parseFromFile dayXParser "day9Testinput.txt"

intParser :: Parser Int
intParser = read <$> some digit <* many space