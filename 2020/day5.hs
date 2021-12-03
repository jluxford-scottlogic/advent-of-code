
import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Functor
import Data.Char (digitToInt)
import Data.List

data SeatPlace = SeatPlace [Int] [Int]
        deriving(Show, Eq)

findSeat :: [SeatPlace] -> Int
findSeat sps = countThrough allSeats where 
    allSeats = sort (fmap calcSeat sps)
    countThrough :: [Int] -> Int
    countThrough [] = -1
    countThrough [a] = -1
    countThrough (a:b:as) | a + 1 == b = countThrough (b:as)
                          | otherwise  = a + 1

calcSeat :: SeatPlace -> Int 
calcSeat (SeatPlace fb lr) = binaryCalc fb * 8 + binaryCalc lr

binaryCalc :: [Int] -> Int
binaryCalc = foldl (\ a b -> a * 2 + b) 0

day5Parser :: Parser [SeatPlace]
day5Parser = some seatPlaceParser <* (eof <|> (char '\n' $> () ))

seatPlaceParser :: Parser SeatPlace
seatPlaceParser = SeatPlace <$> some frontBackParser <*> some leftRightParser <* many space

frontBackParser :: Parser Int
frontBackParser = 0 <$ string "F"
              <|> 1 <$ string "B"

leftRightParser :: Parser Int
leftRightParser = 0 <$ string "L"
              <|> 1 <$ string "R"

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [SeatPlace])
parsedValue = parseFromFile day5Parser "C:/Dev/advent-of-code/2020/day5Input.txt"