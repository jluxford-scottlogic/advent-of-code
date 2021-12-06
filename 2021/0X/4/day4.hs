import Text.ParserCombinators.Parsec
import Control.Applicative (some)

data Bingo = Bingo [Int] [[[Maybe Int]]]
    deriving (Show, Eq)

runBingo :: Bingo -> Int 
runBingo (Bingo [] bbs) = -1
runBingo (Bingo (n:ns) bbs) | validBoards == [] = runBingo (Bingo ns updatedBoards) 
                            | otherwise = n * sumOfBoard where
    updatedBoards = fmap (updateBingoBoard n) bbs
    validBoards = filter testBingo updatedBoards
    winningBoard = head validBoards
    sumOfBoard = sum (fmap (foldl f 0) winningBoard)
    f :: Int -> Maybe Int -> Int
    f n Nothing = n
    f n (Just x) = n + x

runBingoLose :: Bingo -> Int
runBingoLose (Bingo [] bbs) = -1
runBingoLose (Bingo ns [a]) = runBingo (Bingo ns [a])
runBingoLose (Bingo (n:ns) bbs) | validBoards /= [] = runBingoLose (Bingo ns newBoards) 
                                | otherwise = runBingoLose (Bingo ns updatedBoards) where
    updatedBoards = fmap (updateBingoBoard n) bbs
    validBoards = filter testBingo updatedBoards
    winningBoard = head validBoards
    newBoards = filter (not . testBingo) updatedBoards
    sumOfBoard = sum (fmap (foldl f 0) winningBoard)
    f :: Int -> Maybe Int -> Int
    f n Nothing = n
    f n (Just x) = n + x

updateBingoBoard :: Int -> [[Maybe Int]] -> [[Maybe Int]]
updateBingoBoard n = fmap (fmap (f n)) where
    f :: Int -> Maybe Int -> Maybe Int
    f n Nothing = Nothing 
    f n (Just x) | x == n = Nothing
                 | otherwise = Just x

testBingo :: [[Maybe Int]] -> Bool
testBingo bb = rowsTest || columnsTest where
    rowsTest = or (fmap testLine bb)
    columnsTest = or (fmap testLine [fmap (!! x) bb | x <- [0..length bb - 1]])

testLine :: [Maybe Int] -> Bool
testLine [] = True
testLine (Nothing:bs) = testLine bs
testLine (Just x : bs) = False

inputParser :: Parser Bingo
inputParser = Bingo <$> some (intParser <* many (char ',')) <* many space <*> some (bingoBoardParser <* many space) <* eof

bingoBoardParser :: Parser [[Maybe Int]]
bingoBoardParser = some (some (Just <$> intParser) <* char '\n')

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError Bingo)
parsedValue = parseFromFile inputParser "day4input.txt"

parsedTest :: IO (Either ParseError Bingo)
parsedTest = parseFromFile inputParser "day4Testinput.txt"

intParser :: Parser Int
intParser = read <$ many (char ' ') <*> some digit