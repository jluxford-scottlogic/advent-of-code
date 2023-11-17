import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (nub)

data Direction = R | L | U | D
    deriving (Show, Eq)

type ParsedType = [(Direction, Integer)]

unwrap :: ParsedType -> [Direction]
unwrap [] = []
unwrap ((d,i):ds) | i < 1 = unwrap ds
                  | otherwise = d : (unwrap ((d, i - 1):ds))

type Coord = (Integer, Integer)

type HeadTail = (Coord, Coord)

trackHeadTail :: [Direction] -> (HeadTail, [Coord]) -> (HeadTail, [Coord])
trackHeadTail [] htd = htd
trackHeadTail (d:ds) htd = trackHeadTail ds (moveHeadTail d htd)

moveHeadTail :: Direction -> (HeadTail, [Coord]) -> (HeadTail, [Coord])
moveHeadTail R (((hx,hy),(tx,ty)), cs) | hx + 1 > tx + 1 = (((hx + 1,hy),(tx + 1, diffMove hy ty)), (tx + 1, diffMove hy ty):cs)
                                       | otherwise = (((hx + 1, hy),(tx,ty)), cs)
moveHeadTail L (((hx,hy),(tx,ty)), cs) | hx - 1 < tx - 1 = (((hx - 1,hy),(tx - 1, diffMove hy ty)), (tx - 1, diffMove hy ty):cs)
                                       | otherwise = (((hx - 1, hy),(tx,ty)), cs)
moveHeadTail U (((hx,hy),(tx,ty)), cs) | hy + 1 > ty + 1 = (((hx,hy + 1),(diffMove hx tx, ty + 1)), (diffMove hx tx, ty + 1):cs)
                                       | otherwise = (((hx, hy + 1),(tx,ty)), cs)
moveHeadTail D (((hx,hy),(tx,ty)), cs) | hy - 1 < ty - 1 = (((hx,hy - 1),(diffMove hx tx, ty - 1)), (diffMove hx tx, ty - 1):cs)
                                       | otherwise = (((hx, hy - 1),(tx,ty)), cs)

diffMove :: Integer -> Integer -> Integer
diffMove h t | h == t = t
             | h < t = t - 1
             | h > t = t + 1

stage1 :: ParsedType -> Int
stage1 = length . nub . snd . ((flip trackHeadTail) (((0,0),(0,0)), [(0,0)])) . unwrap

stage1Test :: ParsedType -> (HeadTail, [Coord])
stage1Test = ((flip trackHeadTail) (((0,0),(0,0)), [])) . unwrap

trackHeadTail2 :: [Direction] -> ([Coord], [[Coord]]) -> ([Coord], [[Coord]])
trackHeadTail2 [] htd = htd
trackHeadTail2 (d:ds) (ts, css) = trackHeadTail2 ds (recurseTail (moveHead d ts, css))

moveHead :: Direction -> [Coord] -> [Coord]
moveHead R ((x,y):cs) = (x + 1, y) : cs
moveHead L ((x,y):cs) = (x - 1, y) : cs
moveHead U ((x,y):cs) = (x, y + 1) : cs
moveHead D ((x,y):cs) = (x, y - 1) : cs

recurseTail :: ([Coord], [[Coord]]) -> ([Coord], [[Coord]])
recurseTail ([], _)  = ([], [])
recurseTail (ts, []) = (ts, [])
recurseTail ((hx,hy):(tx,ty):ts, cs:css) | hx > tx + 1 || hx < tx - 1 || hy > ty + 1 || hy < ty - 1 = (((hx, hy):ts'), ((diffMove hx tx, diffMove hy ty):cs):css') 
                                         | otherwise = (((hx,hy):(tx,ty):ts), (cs:css)) where
    (ts', css') = recurseTail ((diffMove hx tx, diffMove hy ty):ts, css)

stage2 :: ParsedType -> Int
stage2 = length . nub . last . snd . ((flip trackHeadTail2) (list0s, tail (fmap (:[]) list0s))) . unwrap where
    list0s = take 10 (repeat (0,0))

stage1Using2 :: ParsedType -> Int
stage1Using2 = length . nub . last . snd . ((flip trackHeadTail2) (list0s, tail (fmap (:[]) list0s))) . unwrap where
    list0s = take 2 (repeat (0,0))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedTest2 :: IO (Either ParseError ParsedType)
parsedTest2 = parseFromFile usedParser "test2.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseDirectionDigitPair <* (many newline)) <* eof

parseDirectionDigitPair :: Parser (Direction, Integer)
parseDirectionDigitPair = (,) <$> parseDirection <*> parseDigit

parseDigit :: Parser Integer
parseDigit = read <$> (some (oneOf ['0'..'9']))

parseDirection :: Parser Direction
parseDirection = R <$ string "R" <* many space
             <|> L <$ string "L" <* many space
             <|> U <$ string "U" <* many space
             <|> D <$ string "D" <* many space