import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (elem)

type ParsedType = (Int, [[Cell]])

data Cell = Digit [Int] Int | Symbol Int Char | Space
    deriving (Show, Eq)

validateDigit :: Int -> Cell -> Cell
validateDigit _ (Digit _ n) = Digit [0] n
validateDigit _ c = c

chckValFor :: (Int -> Cell -> Cell) -> [Cell] -> [Cell] -> [Cell]
chckValFor f _ [] = []
chckValFor f (x:(Symbol n _):[]) [a, b] = [f n a, f n b]
chckValFor f (x:(Symbol n s):xs) (a:b:c:as) = f n a : (chckValFor f ((Symbol n s):xs) (f n b : f n c : as))
chckValFor f ((Symbol n _):xs) (a:b:as) = f n a : (chckValFor f xs (f n b : as))
chckValFor f (x:xs) (a:as) = a : (chckValFor f xs as)

checkValidityForward :: (Int -> Cell -> Cell) -> [Cell] -> ([Cell], [[Cell]]) -> ([Cell], [[Cell]])
checkValidityForward f xs (ys, css) = (xs, chckValFor f ys xs : css)

frontBackVal :: (Int -> Cell -> Cell) -> [Cell] -> [Cell]
frontBackVal f [a] = [a]
frontBackVal f ((Symbol n c):b:cs) = (Symbol n c) : (frontBackVal f ((f n b) : cs))
frontBackVal f (a:(Symbol n c):cs) = (f n a) : (frontBackVal f ((Symbol n c) : cs))
frontBackVal f (a:b:cs) = a : (frontBackVal f (b : cs))

checkValidity :: (Int -> Cell -> Cell) -> [[Cell]] -> [[Cell]]
checkValidity f = chckValDownFwdAndRev . chckValDownFwdAndRev . (fmap (frontBackVal f)) where
    chckValDownFwdAndRev = reverse . snd . (foldr (checkValidityForward f) (repeat Space, []))

sumIfValid :: Int -> Bool -> [Cell] -> Int
sumIfValid n True []  = n
sumIfValid n False [] = 0
sumIfValid n b ((Digit [0] k):cs)  = sumIfValid (10 * n + k) True cs
sumIfValid n b ((Digit [] k):cs) = sumIfValid (10 * n + k) b cs
sumIfValid n True (c:cs)  = n + sumIfValid 0 False cs
sumIfValid n False (c:cs) = sumIfValid 0 False cs

stage1 :: ParsedType -> Int
stage1 = sum . (fmap (sumIfValid 0 False)) . (checkValidity validateDigit) . snd

validateIfDigitG :: Int -> Cell -> Cell
validateIfDigitG k (Digit ks n) = Digit (k:ks) n
validateIfDigitG _ c = c

runOnEach :: a -> [(a -> b)] -> [b]
runOnEach a [] = []
runOnEach a (f:fs) = f a : (runOnEach a fs)

mapIfValid :: Int -> [Int] -> [Cell] -> Int -> [Int]
mapIfValid n is [] i | elem i is = [n]
                     | otherwise = []
mapIfValid n js ((Digit is k):cs) i = mapIfValid (10 * n + k) (is ++ js) cs i
mapIfValid n is (c:cs) i | elem i is = n : (mapIfValid 0 [] cs i)
                         | otherwise = mapIfValid 0 [] cs i

genGearMap :: [[Cell]] -> Int -> [Int]
genGearMap [] _ = []
genGearMap (gs:gss) n = (mapIfValid 0 [] gs n) ++ (genGearMap gss n)

validGears :: [Int] -> Int
validGears [a,b] = a * b
validGears _ = 0

combValidGears :: [[Int]] -> Int
combValidGears = sum . (fmap validGears)

stage2 :: ParsedType -> Int
stage2 (g, css) = combValidGears (fmap (genGearMap (checkValidity validateIfDigitG css)) [1..g])

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (parserInput 1) <* eof

parserInput :: Int -> Parser ParsedType
parserInput n = someWithThread n (\n -> (parsedRow n) <* spaces)

parsedRow :: Int -> Parser (Int, [Cell])
parsedRow n = someWithThread n parseCell

someWithThread :: a -> (a -> Parser (a, b)) -> Parser (a, [b])
someWithThread a p = (try someP) <|> ((fmap (:[])) <$> p a) where
    someP = do
        (a', b) <- p a
        (a'', bs) <- someWithThread a' p
        return (a'', b:bs)

parseCell :: Int -> Parser (Int, Cell)
parseCell n = ((n,) . (Digit [])) <$> parseDigit
          <|> (n, Space) <$ char '.'
          <|> ((n + 1,) . (Symbol n)) <$> char '*'
          <|> ((n,) . (Symbol 0)) <$> noneOf ('.':'*':'\r':'\n':' ':['0'..'9'])

parseDigit :: Parser Int
parseDigit = (read . (:[])) <$> (oneOf ['0'..'9'])