import Text.ParserCombinators.Parsec
import Control.Applicative (some)

growthFish :: [Integer] -> Int -> Integer
growthFish someDays n = someDays !! n

-- Hard coded values calculated by afterXDays for the speedup
someDays80 :: [Integer]
someDays80 = [1421,1401,1191,1154,1034,950,905,779,768,642,633]

someDays256 :: [Integer]
someDays256 = [6703087164,6206821033,5617089148,5217223242,4726100874,4368232009,3989468462,3649885552,3369186778,3053201612,2837634255,2563887536]

afterXDays :: Int -> [Integer]
afterXDays n | n == 0 = [1, 0, 0, 0, 0, 0, 0, 0, 0]
                 | otherwise = f (afterXDays (n - 1)) where
    f :: [Integer] -> [Integer]
    f (n:ns) = take 6 ns ++ g n (drop 6 ns)
    g :: Integer -> [Integer] -> [Integer]
    g n (s:ss) = n + s : (ss ++ [n])

allFish80 :: [Int] -> Integer
allFish80 = sum . fmap (growthFish someDays80)

allFish256 :: [Int] -> Integer
allFish256 = sum . fmap (growthFish someDays256)

inputParser :: Parser [Int]
inputParser = some (integerParser <* (many space <* char ',' <|> many space <* eof))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Int])
parsedValue = parseFromFile inputParser "day6input.txt"

parsedTest :: IO (Either ParseError [Int])
parsedTest = parseFromFile inputParser "day6Testinput.txt"

integerParser :: Parser Int
integerParser = read <$> some digit <* many space