import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (sort)

calculateQuickestBus :: (Int, [Maybe Int]) -> (Int, Int)
calculateQuickestBus (x, ns) = minimum (fmap f ns) where
    f :: Maybe Int -> (Int, Int)
    f Nothing = (x, 0)
    f (Just n) = (g x n n, n)
    g :: Int -> Int -> Int -> Int
    g a b c | b >= a    = b - a
            | otherwise = g a (b + c) c

stage1 :: (Int, [Maybe Int]) -> Int
stage1 a = f * s where
    (f,s) = calculateQuickestBus a

calculateEarliestTime :: [(Integer, Integer)] -> Integer
calculateEarliestTime ns = incrementTest s 1 where
    s = reverse $ sort ns
    incrementTest :: [(Integer, Integer)] -> Integer -> Integer
    incrementTest ((x, y):ns) mult | testValues ((x * mult) - y) ns = (x * mult) - y
                                   | otherwise = incrementTest ((x,y):ns) (mult + 1)
    testValues :: Integer -> [(Integer, Integer)] -> Bool
    testValues _ [] = True
    testValues testValue ((x, y): ns) | mod (testValue + y) x /= 0 = False
                                      | otherwise = testValues testValue ns

giveTimeStamp :: (Int, [Maybe Int]) -> [(Integer, Integer)]
giveTimeStamp = fst . foldl f ([],0) . snd where
    f :: ([(Integer, Integer)], Int) -> Maybe Int -> ([(Integer, Integer)], Int)
    f (ns, n) Nothing = (ns, n + 1)
    f (ns, n) (Just x) = ((toInteger x, toInteger n):ns, n + 1)

stage2 :: (Int, [Maybe Int]) -> Integer
stage2 = calculateEarliestTime . giveTimeStamp

inputParser :: Parser (Int, [Maybe Int])
inputParser = (,) <$> intParser <*> some (busParser <* many (char ',') <* many space) <* eof

busParser :: Parser (Maybe Int)
busParser = Nothing <$ char 'x'
         <|> Just <$> intParser

intParser :: Parser Int
intParser = read <$ many space <*> some digit <* many space

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError (Int, [Maybe Int]))
parsedValue = parseFromFile inputParser "day13Input.txt"

parsedTest :: IO (Either ParseError (Int, [Maybe Int]))
parsedTest = parseFromFile inputParser "day13TestInput.txt"