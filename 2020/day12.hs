{-# LANGUAGE DeriveFunctor #-}
import Text.ParserCombinators.Parsec
import Control.Applicative (some)

data Action a = N a | S a | E a | W a | L a | R a | F a
    deriving (Show, Eq, Functor)

runAction :: [Action Int] -> (Int, Int)
runAction = fst . foldl f ((0, 0), E ()) where
    f :: ((Int, Int), Action ()) -> Action Int -> ((Int, Int), Action ())
    f ((x, y), action) (N a) = ((x + a, y), action)
    f ((x, y), action) (S a) = ((x - a, y), action)
    f ((x, y), action) (E a) = ((x, y + a), action)
    f ((x, y), action) (W a) = ((x, y - a), action)
    f ((x, y), action) (F a) = f ((x, y), action) (fmap (const a) action)
    f ((x, y), action) rotat = ((x, y), rotateAction action rotat)

rotateAction :: Action a -> Action Int -> Action a
rotateAction (N a) (L n) | n <= 0 = N a
                         | otherwise = rotateAction (W a) (L (n - 90))
rotateAction (S a) (L n) | n <= 0 = S a
                         | otherwise = rotateAction (E a) (L (n - 90))
rotateAction (E a) (L n) | n <= 0 = E a
                         | otherwise = rotateAction (N a) (L (n - 90))
rotateAction (W a) (L n) | n <= 0 = W a
                         | otherwise = rotateAction (S a) (L (n - 90))
rotateAction (N a) (R n) | n <= 0 = N a
                         | otherwise = rotateAction (E a) (R (n - 90))
rotateAction (S a) (R n) | n <= 0 = S a
                         | otherwise = rotateAction (W a) (R (n - 90))
rotateAction (E a) (R n) | n <= 0 = E a
                         | otherwise = rotateAction (S a) (R (n - 90))
rotateAction (W a) (R n) | n <= 0 = W a
                         | otherwise = rotateAction (N a) (R (n - 90))
rotateAction a _ = a

stage1 :: [Action Int] -> Int
stage1 = uncurry (+) . (\(a,b) -> (abs a, abs b)) . runAction

waypointAction :: [Action Int] -> (Int, Int)
waypointAction = fst . foldl f ((0, 0), (1, 10)) where
    f :: ((Int, Int), (Int, Int)) -> Action Int -> ((Int, Int), (Int, Int))
    f ((x, y), (wx, wy)) (N a) = ((x, y), (wx + a, wy))
    f ((x, y), (wx, wy)) (S a) = ((x, y), (wx - a, wy))
    f ((x, y), (wx, wy)) (E a) = ((x, y), (wx, wy + a))
    f ((x, y), (wx, wy)) (W a) = ((x, y), (wx, wy - a))
    f ((x, y), (wx, wy)) (F a) = ((x + wx * a, y + wy * a), (wx, wy))
    f ((x, y), waypoint) rotat = ((x, y), rotateWaypoint waypoint rotat)

rotateWaypoint :: (Int, Int) -> Action Int -> (Int, Int)
rotateWaypoint (x, y) (L n) | n <= 0 = (x, y)
                         | otherwise = rotateWaypoint (y, -x) (L (n - 90))
rotateWaypoint (x, y) (R n) | n <= 0 = (x, y)
                         | otherwise = rotateWaypoint (-y, x) (R (n - 90))
rotateWaypoint a _ = a

stage2 :: [Action Int] -> Int
stage2 = uncurry (+) . (\(a,b) -> (abs a, abs b)) . waypointAction

inputParser :: Parser [Action Int]
inputParser = some actionParser <* eof

actionParser :: Parser (Action Int)
actionParser = N <$ char 'N' <*> intParser
           <|> S <$ char 'S' <*> intParser
           <|> E <$ char 'E' <*> intParser
           <|> W <$ char 'W' <*> intParser
           <|> L <$ char 'L' <*> intParser
           <|> R <$ char 'R' <*> intParser
           <|> F <$ char 'F' <*> intParser

intParser :: Parser Int
intParser = read <$ many space <*> some digit <* many space

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Action Int])
parsedValue = parseFromFile inputParser "day12Input.txt"

parsedTest :: IO (Either ParseError [Action Int])
parsedTest = parseFromFile inputParser "day12TestInput.txt"