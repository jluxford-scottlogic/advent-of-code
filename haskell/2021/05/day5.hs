import Text.ParserCombinators.Parsec
import Control.Applicative (some)

-- This is a terrible way of doing this and takes a very long time to run
searchAllForDups :: [(Int, Int)] -> Int
searchAllForDups [] = 0
searchAllForDups (c:cs) | c `elem` cs = 1 + searchAllForDups (filter (/=c) cs)
                        | otherwise  = searchAllForDups cs

genAll :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
genAll ((x1, y1), (x2, y2)) | not (x1 == x2 || y1 == y2) = [((x * sign (x2 - x1)) + x1, (x * sign (y2 - y1)) + y1) | x <- [0..xDiff]]
                            | otherwise = inc ++ dec where
    inc = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
    dec = [(x, y) | x <- [x2..x1], y <- [y2..y1]]
    xDiff = abs (x1 - x2)
    sign :: Int -> Int
    sign x = div x (abs x)


keepHorAndVert :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
keepHorAndVert = filter f where
    f :: ((Int, Int), (Int, Int)) -> Bool
    f ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

totalCells :: [((Int, Int), (Int, Int))] -> Int
totalCells as = xLen * yLen where
    xLen = maximum (fmap (\((x1,_),(x2,_)) -> max x1 x2) as) + 1
    yLen = maximum (fmap (\((_,y1),(_,y2)) -> max y1 y2) as) + 1


calcStage1 :: [((Int, Int), (Int, Int))] -> Int
calcStage1 = searchAllForDups . concatMap genAll . keepHorAndVert

calcStage2 :: [((Int, Int), (Int, Int))] -> Int
calcStage2 = searchAllForDups . concatMap genAll

inputParser :: Parser [((Int, Int), (Int, Int))]
inputParser = some lineParser <* eof

lineParser :: Parser ((Int, Int), (Int, Int))
lineParser = (,) <$> coordParser <* string "->" <* many space <*> coordParser

coordParser :: Parser (Int, Int)
coordParser = (,) <$> intParser <* char ',' <*> intParser

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [((Int, Int), (Int, Int))])
parsedValue = parseFromFile inputParser "day5input.txt"

parsedTest :: IO (Either ParseError [((Int, Int), (Int, Int))])
parsedTest = parseFromFile inputParser "day5Testinput.txt"

intParser :: Parser Int
intParser = read <$> some digit <* many space