import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Bits
import MoveArr

type ParsedType = [[Integer]]

type ArrWithBitCount = MoveArr (Integer, Integer)

setupArr :: [[Integer]] -> MoveArr (Integer, Integer)
setupArr = fromArr . (fmap (fmap (,0)))

setNineBitCount :: Int -> MoveArr (Integer, Integer) -> MoveArr (Integer, Integer)
setNineBitCount c xss | atEnd xss && fst (peekArr X 0 xss) /= 9 = stepA X (-(lengthArr X xss)) (stepA Y (-(lengthArr Y xss)) xss)
                      | atEnd xss && fst (peekArr X 0 xss) == 9 =  stepA X (-(lengthArr X xss)) (stepA Y (-(lengthArr Y xss)) (stepThrough (updateLoc (fmap (const (bit c))) xss)))
                      | fst (peekArr X 0 xss) == 9 = setNineBitCount (c + 1) (stepThrough (updateLoc (fmap (const (bit c))) xss))
                      | otherwise = setNineBitCount c (stepThrough xss)

bitOr :: Integer -> [(Integer, Integer)] -> Integer
bitOr _ [] = 0
bitOr n ((x,b):xs) | n + 1 == x = b .|. bitOr n xs
                   | otherwise = bitOr n xs

calcDynaProg ::  (Integer -> [(Integer, Integer)] -> Integer) -> Integer -> MoveArr (Integer, Integer) -> MoveArr (Integer, Integer)
calcDynaProg f n xss | n < 0 = xss
                     | atEnd xss && fst (peekArr X 0 xss) /= n = calcDynaProg f (n - 1) (stepA X (-(lengthArr X xss)) (stepA Y (-(lengthArr Y xss)) xss))
                     | atEnd xss && fst (peekArr X 0 xss) == n = calcDynaProg f (n - 1) (stepA X (-(lengthArr X xss)) (stepA Y (-(lengthArr Y xss)) (stepThrough (updateLoc (fmap (const (peekAround (f n) xss))) xss))))
                     | fst (peekArr X 0 xss) == n = calcDynaProg f n (stepThrough (updateLoc (fmap (const (peekAround (f n) xss))) xss))
                     | otherwise = calcDynaProg f n (stepThrough xss)

bitCountZeros :: MoveArr (Integer, Integer) -> Int
bitCountZeros = sum . (fmap (popCount . snd)) . filter ((== 0) . fst) . concat . toArr

stage1 :: [[Integer]] -> Int
stage1 = bitCountZeros . calcDynaProg bitOr 8 . setNineBitCount 0 . setupArr

setUpArrNine :: [[Integer]] -> MoveArr (Integer, Integer)
setUpArrNine = fromArr . (fmap (fmap setOneOnNine)) where
    setOneOnNine :: Integer -> (Integer, Integer)
    setOneOnNine 9 = (9, 1)
    setOneOnNine n = (n, 0)

countIfClose :: Integer -> [(Integer, Integer)] -> Integer
countIfClose _ [] = 0
countIfClose n ((x,b):xs) | n + 1 == x = b + countIfClose n xs
                          | otherwise = countIfClose n xs

countZeros :: MoveArr (Integer, Integer) -> Integer
countZeros = sum . (fmap snd) . filter ((== 0) . fst) . concat . toArr

stage2 :: [[Integer]] -> Integer
stage2 = countZeros . calcDynaProg countIfClose 8 . setUpArrNine

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseLine <* (many space)) <* eof

parseLine :: Parser [Integer]
parseLine = some parseDigit

parseDigit :: Parser Integer
parseDigit = (read . (:[])) <$> (oneOf ['0'..'9'])