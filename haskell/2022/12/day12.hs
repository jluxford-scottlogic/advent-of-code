import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (transpose)

type ParsedType = [[Either Int StartEnd]]

data StartEnd = Start | End
    deriving (Show, Eq)

data Cell = Cell (Either Int StartEnd) (Maybe Int)
    deriving (Show, Eq)

doAllDirections :: ([a] -> [a]) -> [[a]] -> [[a]]
doAllDirections f ms = fmap reverse (transpose dir4) where
    dir1 = fmap f ms
    dir2 = fmap (f . reverse) dir1
    dir3 = fmap f (transpose dir2)
    dir4 = fmap (reverse . f . reverse) dir3

updateMap :: [[Cell]] -> [[Cell]]
updateMap = doAllDirections f where
    f :: [Cell] -> [Cell]
    f [] = []
    f (c:[]) = [c]
    f (c1@(Cell h1 (Nothing)):c2@(Cell h2 (Just n)):cs) | canStep h1 h2 = (Cell h1 (Just (n + 1))) : (f (c2:cs))
                                                        | otherwise = c1 : (f (c2:cs))
    f (c1@(Cell h1 (Just m)):c2@(Cell h2 (Just n)):cs)  | (canStep h1 h2) && (m > (n + 1)) = (Cell h1 (Just (n + 1))) : (f (c2:cs))
                                                        | otherwise = c1 : (f (c2:cs))
    f (c1:c2:cs) = c1 : (f (c2:cs))

fillMap :: [[Cell]] -> [[Cell]]
fillMap css = fillMapHelper ((length css) * (maximum (fmap length css))) css where
    fillMapHelper :: Int -> [[Cell]] -> [[Cell]]
    fillMapHelper 0 = id
    fillMapHelper n = (fillMapHelper (n - 1)) . updateMap

find :: StartEnd -> ParsedType -> (Int, Int)
find = findHelper 0 0

findHelper :: Int -> Int -> StartEnd -> ParsedType -> (Int, Int)
findHelper x y se ([]:css) = findHelper 0 (y + 1) se css
findHelper x y se (((Right se2):cs):css) | se == se2 = (x, y)
                                   | otherwise = findHelper (x + 1) y se (cs:css)
findHelper x y se ((_:cs):css) = findHelper (x + 1) y se (cs:css)

heightOfCell :: (Either Int StartEnd) -> Int
heightOfCell (Left n) = n
heightOfCell (Right Start) = 0
heightOfCell (Right End) = 25

canStep :: (Either Int StartEnd) -> (Either Int StartEnd) -> Bool
canStep c d = (heightOfCell c) <= ((heightOfCell d) + 1)

constructCells :: [[Either Int StartEnd]] -> [[Cell]]
constructCells = fmap (fmap f) where
    f :: (Either Int StartEnd) -> Cell
    f (Left n) = Cell (Left n) Nothing
    f (Right Start) = Cell (Right Start) (Just 0)
    f (Right End) = Cell (Right End) Nothing

findEndVal :: [[Cell]] -> Int
findEndVal [] = 0
findEndVal ([]:css) = findEndVal css
findEndVal ((Cell (Right End) (Just n):cs):css) = n
findEndVal ((c:cs):css) = findEndVal (cs:css)

stage1 :: ParsedType -> Int
stage1 = findEndVal . fillMap . constructCells

constructCells2 :: [[Either Int StartEnd]] -> [[Cell]]
constructCells2 = fmap (fmap f) where
    f :: (Either Int StartEnd) -> Cell
    f (Left n) | n == 0 = Cell (Right Start) (Just 0)
               | otherwise = Cell (Left n) Nothing
    f (Right Start) = Cell (Right Start) (Just 0)
    f (Right End) = Cell (Right End) Nothing

stage2 :: ParsedType -> Int
stage2 = findEndVal . fillMap . constructCells2

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some ((parseRow) <* many space) <* eof

parseRow :: Parser [Either Int StartEnd]
parseRow = some (parseCell)

parseCell :: Parser (Either Int StartEnd)
parseCell = Left <$> parseHeight
        <|> Right <$> parseStartEnd

convertAlphaToInt :: Eq a => [a] -> a -> Int
convertAlphaToInt (a:as) b | a == b = 0
                           | otherwise = 1 + (convertAlphaToInt as b)

parseHeight :: Parser Int
parseHeight = convertAlphaToInt ['a'..'z'] <$> (oneOf (['a'..'z']))

parseStartEnd :: Parser StartEnd
parseStartEnd = Start <$ string "S"
            <|> End <$ string "E"