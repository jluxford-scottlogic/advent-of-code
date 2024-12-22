import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (nub)
import GHC.Exts (groupWith)

type ParsedType = [[Maybe Char]]

convertToCoordList :: (Int, Int) -> [[Maybe Char]] -> ((Int, Int), [(Char, (Int, Int))])
convertToCoordList (x,y) [[(Just c)]] = ((x,y), [(c, (x,y))])
convertToCoordList (x,y) [[Nothing]] = ((x,y), [])
convertToCoordList (x,y) ([]:css) = convertToCoordList (0, y+1) css
convertToCoordList (x,y) (((Just c):cs):css) = fmap ((c, (x,y)):) (convertToCoordList (x + 1, y) (cs:css))
convertToCoordList (x,y) (((Nothing):cs):css) = convertToCoordList (x + 1, y) (cs:css)

groupUp :: [(Char, (Int, Int))] -> [[(Int, Int)]]
groupUp css = fmap (fmap (snd)) (groupWith fst css)

calcAntiNodes :: ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)]
calcAntiNodes f xs = concat [f x y | x <- xs, y <- xs, x /= y]

diffAnti :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
diffAnti (x,y) (a,b) = [(x + x - a, y + y - b)]

limToWithinLim :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
limToWithinLim (limX, limY) = filter (\(x,y) -> x <= limX && y <= limY && x >= 0 && y >= 0)

stage1 :: ParsedType -> Int
stage1 css = (length . nub . (limToWithinLim lim) . concat . (fmap (calcAntiNodes diffAnti)) . groupUp) coords where
    (lim, coords) = convertToCoordList (0,0) css

harmAnti :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
harmAnti (xLim, yLim) (x,y) (a,b) = [(x + k * (x - a), y + k * (y - b)) | k <- [0..minToLim]] where
    minToLim = min (limFind xLim x a) (limFind yLim y b)

limFind :: Int -> Int -> Int -> Int
limFind lim x a | x > a = div (lim - x) (x - a)
                | otherwise = div x (a - x)

stage2 :: ParsedType -> Int
stage2 css = (length . nub . concat . (fmap (calcAntiNodes (harmAnti lim))) . groupUp) coords where
    (lim, coords) = convertToCoordList (0,0) css

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some ((some parseCell) <* (many space)) <* eof

parseCell :: Parser (Maybe Char)
parseCell = Nothing <$ string "."
        <|> Just <$> parseChar

parseChar :: Parser Char
parseChar = oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']) 