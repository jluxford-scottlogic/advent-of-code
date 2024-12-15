import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [(Int, [Cubes])]

data Cubes = End | Blue Int Cubes | Red Int Cubes | Green Int Cubes
    deriving (Show, Eq)

cubesToTuple :: Cubes -> (Int, Int, Int)
cubesToTuple End = (0,0,0)
cubesToTuple (Blue n cs)  = (n, r, g) where
    (b, r, g) = cubesToTuple cs
cubesToTuple (Red n cs)   = (b, n, g) where
    (b, r, g) = cubesToTuple cs
cubesToTuple (Green n cs) = (b, r, n) where
    (b, r, g) = cubesToTuple cs

addIfTrue :: (Int, Bool) -> Int -> Int
addIfTrue (n, True)  k = n + k
addIfTrue (_, False) k = k

calcIfLessThan :: (Int, Int, Int) -> [(Int, Int, Int)] -> Bool
calcIfLessThan _ [] = True
calcIfLessThan (bm, rm, gm) ((b, r, g):cs) = b <= bm && r <= rm && g <= gm && (calcIfLessThan (bm, rm, gm) cs)

stage1 :: ParsedType -> Int
stage1 = foldr addIfTrue 0 . (fmap (fmap ((calcIfLessThan (14, 12, 13)) . (fmap cubesToTuple))))

fewestNumber :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fewestNumber (b, r, g) (bm, rm, gm) = (max b bm, max r rm, max g gm)

power :: (Int, Int, Int) -> Int
power (b, r, g) = b * r * g

stage2 :: ParsedType -> Int
stage2 = sum . (fmap (power . (foldr fewestNumber (0, 0, 0)) . (fmap cubesToTuple))) . (fmap snd)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseGame <* spaces) <* eof

parseGame :: Parser (Int, [Cubes])
parseGame = (,) <$ string "Game" <* spaces <*> parseNum <* string ":" <* space <*> (some (parseCubes))

parseCubes :: Parser Cubes
parseCubes = try ((parseNum <* spaces) >>= parseCubeVal)
         <|> End <$ ((char ';' <* space) <|> endOfLine)

parseCubeVal :: Int -> Parser Cubes
parseCubeVal n = (Blue n)  <$ string "blue"  <* (many ((char ',') <* spaces)) <*> parseCubes
             <|> (Red n)   <$ string "red"   <* (many ((char ',') <* spaces)) <*> parseCubes
             <|> (Green n) <$ string "green" <* (many ((char ',') <* spaces)) <*> parseCubes

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))