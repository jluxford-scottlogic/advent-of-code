import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)

type ParsedType = [[Int]]

checkSafe :: [Int] -> Bool
checkSafe (x:y:xs) = ((x > y) && checkDec (x:y:xs)) ||
                     ((y > x) && checkInc (x:y:xs))
checkSafe _ = True

checkDec :: [Int] -> Bool
checkDec (x:y:xs) = (x - y < 4) && (x - y > 0) && (checkDec (y:xs))
checkDec _ = True

checkInc :: [Int] -> Bool
checkInc (x:y:xs) = (y - x < 4) && (y - x > 0) && (checkInc (y:xs))
checkInc _ = True

bitShow :: Bool -> Int
bitShow True = 1
bitShow False = 0

checkSafeDamp :: [Int] -> Bool
checkSafeDamp (x:y:xs) = (x > y && (x - y < 4) && (x - y > 0) && checkDecDamp (x:y:xs)) ||
                         (y > x && (y - x < 4) && (y - x > 0) && checkIncDamp (x:y:xs)) ||
                         checkSafe (x:xs) || checkSafe (y:xs)
checkSafeDamp _ = True

checkDecDamp :: [Int] -> Bool
checkDecDamp (z:x:y:xs) | (x - y < 4) && (x - y > 0) = (checkDecDamp (x:y:xs))
                        | otherwise = checkDec (x:xs) || checkDec (z:y:xs)
checkDecDamp _ = True

checkIncDamp :: [Int] -> Bool
checkIncDamp (z:x:y:xs) | (y - x < 4) && (y - x > 0) = (checkIncDamp (x:y:xs))
                        | otherwise = checkInc (x:xs) || checkInc (z:y:xs)
checkIncDamp _ = True

stage1 :: [[Int]] -> Int
stage1 = sum . (fmap (bitShow . checkSafe))

stage2 :: [[Int]] -> Int
stage2 = sum . (fmap (bitShow . checkSafeDamp))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseReport <* spaces) <* eof

parseReport :: Parser [Int]
parseReport = some (parseNum <* many (char ' '))

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))