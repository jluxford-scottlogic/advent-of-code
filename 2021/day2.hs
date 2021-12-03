import Text.ParserCombinators.Parsec
import Control.Applicative (some)

data Command = Forward Int | Backward Int | Up Int | Down Int

multCoord :: (Int, Int) -> Int 
multCoord (a, b) = a * b

runCommand :: [Command] -> (Int, Int)
runCommand = foldl f (0, 0) where
    f :: (Int, Int) -> Command -> (Int, Int)
    f (x, z) (Forward n) = (x + n, z)
    f (x, z) (Backward n) = (x - n, z)
    f (x, z) (Up n) = (x, z - n)
    f (x, z) (Down n) = (x, z + n)

multCoord3 :: (Int, Int, Int) -> Int 
multCoord3 (a, c, b) = a * b

runCommandWithAim :: [Command] -> (Int, Int, Int)
runCommandWithAim = foldl f (0, 0, 0) where
    f :: (Int, Int, Int) -> Command -> (Int, Int, Int)
    f (x, a, z) (Forward n) = (x + n, a, z + (a * n))
    f (x, a, z) (Backward n) = (x - n, a, z - (a * n))
    f (x, a, z) (Up n) = (x, a - n, z)
    f (x, a, z) (Down n) = (x, a + n, z)

day2Parser :: Parser [Command]
day2Parser = some (commandParser <* many space) <* eof

commandParser :: Parser Command
commandParser = try (Forward <$ string "forward " <*> intParser)
            <|> try (Backward <$ string "backward " <*> intParser)
            <|> try (Up <$ string "up " <*> intParser)
            <|> try (Down <$ string "down " <*> intParser)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Command])
parsedValue = parseFromFile day2Parser "C:/Dev/advent-of-code/2021/day2input.txt"

parsedTest :: IO (Either ParseError [Command])
parsedTest = parseFromFile day2Parser "C:/Dev/advent-of-code/2021/day2Testinput.txt"

intParser :: Parser Int
intParser = read <$> some digit <* many space