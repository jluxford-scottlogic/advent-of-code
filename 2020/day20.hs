
import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Functor
import Data.List

data TileFull = TileFull Int [[Bool]]
    deriving (Show, Eq)

data Tile = Tile Int [[Bool]]
    deriving (Show, Eq)

reduceTile :: TileFull -> Tile
reduceTile (TileFull n bs) = Tile n [north, east, south, west] where
    north = head bs
    south = last bs
    east = fmap last bs
    west = fmap head bs

rotations :: [[Bool]] -> [[[Bool]]]
rotations t@[n, e, s, w] = [t, r, r2, r3, f, rf, r2f, r3f] where
    r = [reverse w, n, reverse e, s]
    r2 = [reverse s, reverse w, reverse n, reverse e]
    r3 = [e, reverse s, w, reverse n]
    f = [reverse n, w, reverse s, e]
    rf = [reverse e, reverse n, reverse w, reverse s]
    r2f = [s, reverse e, n, reverse w]
    r3f = [w, s, e, n]

calcEdgesCheap :: [Tile] -> Int
calcEdgesCheap ts = product (fmap (\(Tile n _) -> n) (filter (valid codes) ts)) where
    codes :: [[Bool]]
    codes = concatMap (\(Tile n bs) -> bs ++ fmap reverse bs) ts
    valid :: [[Bool]] -> Tile -> Bool
    valid codes tile@(Tile i bs) = length xTest == 2 where
        x = [length (filter (== b) codes) | b <- bs]
        xTest = filter (== 1) x

combineTiles :: [Tile] -> [[Tile]]
combineTiles bs = head (slotTiles bs centreBottom)  where
    centre = [[ [] | y <- [0..squareRoot]] | x <- [0..squareRoot]]
    centreEast = fmap (\a -> init a ++ [1:last a]) centre
    centreWest = fmap (\a -> (4 : head a) : tail a) centreEast
    centreTop = fmap (fmap (1:)) head centreWest : tail centreWest
    centreBottom = init centreTop ++ [fmap (fmap (3:)) last centreTop]
    squareRoot = sqrtExact (length bs) 2 - 1
    sqrtExact :: Int -> Int -> Int
    sqrtExact n k | k * k == n = k
                  | k > n = - 1
                  | otherwise = sqrtExact n (k + 1)

slotTiles :: [Tile] -> [[[Int]]] -> [[[Tile]]]
slotTiles _ _ = [[[]]]

growOutTiles :: [Tile] -> [[Maybe Tile]] -> [[([Bool], Int)]] -> [[Maybe Tile]]
growOutTiles [] ts [] = ts
growOutTiles ((Tile n as):ts) [] [] = growOutTiles ts [[Just $ Tile n as]] tileOptions where
    tileOptions :: [[([Bool], Int)]]
    tileOptions = [[(head as, 2)], [(as !! 1, 3)], [(as !! 2, 0)], [(as !! 3, 1)]]
growOutTiles ((Tile n as):ts) board options = [] where 
    placement = head (compare options (rotations as))
    compare :: [[([Bool], Int)]] -> [[[Bool]]] -> [(Int, Int)]
    compare _ _ = undefined

day20Parser :: Parser [TileFull]
day20Parser = some (parseTile <* many space) <* eof

parseTile :: Parser TileFull
parseTile = TileFull <$ string "Tile " <*> intParser <* string ":\n" <*> some (parseTileLine <* some space)

parseTileLine :: Parser [Bool]
parseTileLine = some parseDotOrHash where
    parseDotOrHash :: Parser Bool
    parseDotOrHash = char '.' $> True <|> char '#' $> False


runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [TileFull])
parsedValue = parseFromFile day20Parser "C:/Dev/advent-of-code/2020/day20Input.txt"

parsedTest :: IO (Either ParseError [TileFull])
parsedTest = parseFromFile day20Parser "C:/Dev/advent-of-code/2020/day20testinput.txt"

intParser :: Parser Int
intParser = read <$> some digit