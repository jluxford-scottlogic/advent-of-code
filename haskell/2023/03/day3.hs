import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import Data.List (elem)

type ParsedType = [[Cell]]

data Cell = Digit Bool Int | Symbol Char | Space
    deriving (Show, Eq)

showCells :: [Cell] -> String
showCells [] = ""
showCells ((Digit True n):cs) = show n ++ (showCells cs)
showCells ((Digit False n):cs) = '_' : (showCells cs)
showCells ((Symbol c):cs) = c : (showCells cs)
showCells ((Space):cs) = '.' : (showCells cs)

validateIfDigit :: Cell -> Cell
validateIfDigit (Digit _ n) = Digit True n
validateIfDigit c = c

chckValFor :: [Cell] -> [Cell] -> [Cell]
chckValFor _ [] = []
chckValFor (x:(Symbol _):[]) [a, b] = [validateIfDigit a, validateIfDigit b]
chckValFor (x:(Symbol s):xs) (a:b:c:as) = validateIfDigit a : (chckValFor ((Symbol s):xs) (validateIfDigit b : validateIfDigit c : as))
chckValFor ((Symbol _):xs) (a:b:as) = validateIfDigit a : (chckValFor xs (validateIfDigit b : as))
chckValFor (x:xs) (a:as) = a : (chckValFor xs as)

checkValidityForward :: [Cell] -> ([Cell], [[Cell]]) -> ([Cell], [[Cell]])
checkValidityForward xs (ys, css) = (xs, chckValFor ys xs : css)

frontBackVal :: [Cell] -> [Cell]
frontBackVal [a] = [a]
frontBackVal ((Symbol c):b:cs) = (Symbol c) : (frontBackVal ((validateIfDigit b) : cs))
frontBackVal (a:(Symbol c):cs) = (validateIfDigit a) : (frontBackVal ((Symbol c) : cs))
frontBackVal (a:b:cs) = a : (frontBackVal (b : cs))

checkValidity :: [[Cell]] -> [[Cell]]
checkValidity = chckValDownFwdAndRev . chckValDownFwdAndRev . (fmap frontBackVal) where
    chckValDownFwdAndRev = reverse . snd . (foldr checkValidityForward (repeat Space, []))

sumIfValid :: Int -> Bool -> [Cell] -> Int
sumIfValid n True []  = n
sumIfValid n False [] = 0
sumIfValid n b ((Digit True k):cs)  = sumIfValid (10 * n + k) True cs
sumIfValid n b ((Digit False k):cs) = sumIfValid (10 * n + k) b cs
sumIfValid n True (c:cs)  = n + sumIfValid 0 False cs
sumIfValid n False (c:cs) = sumIfValid 0 False cs

stage1Test :: ParsedType -> [String]
stage1Test = (fmap showCells) . checkValidity

stage1 :: ParsedType -> Int
stage1 = sum . (fmap (sumIfValid 0 False)) . checkValidity

data Gear = DigitG [Int] Int | Gear Int | NotGear
    deriving (Show, Eq)

showGears :: [Gear] -> String
showGears [] = ""
showGears ((DigitG (n:ns) _):cs) = show n ++ (showGears cs)
showGears ((DigitG [] _):cs) = '_' : (showGears cs)
showGears ((Gear n):cs) = (show n) ++ (showGears cs)
showGears ((NotGear):cs) = '.' : (showGears cs)

convCellToGears :: Cell -> (Int, [Gear]) -> (Int, [Gear])
convCellToGears (Digit _ n) (g, gs) = (g, DigitG [] n : gs)
convCellToGears (Symbol '*') (g, gs) = (g + 1, Gear g : gs)
convCellToGears (Symbol _) (g, gs) = (g, NotGear : gs)
convCellToGears (Space) (g, gs) = (g, NotGear : gs)

convAllCellsToGears :: Int -> [[Cell]] -> (Int, [[Gear]])
convAllCellsToGears g [] = (g, [])
convAllCellsToGears g (cs:css) = (g'', gs : gss) where
    (g', gs) = foldr convCellToGears (g, []) cs
    (g'', gss) = convAllCellsToGears g' css

validateIfDigitG :: Int -> Gear -> Gear
validateIfDigitG k (DigitG ks n) = DigitG (k:ks) n
validateIfDigitG _ c = c

chckValForG :: [Gear] -> [Gear] -> [Gear]
chckValForG _ [] = []
chckValForG (x:(Gear n):[]) [a, b] = [validateIfDigitG n a, validateIfDigitG n b]
chckValForG (x:(Gear n):xs) (a:b:c:as) = validateIfDigitG n a : (chckValForG ((Gear n):xs) (validateIfDigitG n b : validateIfDigitG n c : as))
chckValForG ((Gear n):xs) (a:b:as) = validateIfDigitG n a : (chckValForG xs (validateIfDigitG n b : as))
chckValForG (x:xs) (a:as) = a : (chckValForG xs as)

checkValidityForwardG :: [Gear] -> ([Gear], [[Gear]]) -> ([Gear], [[Gear]])
checkValidityForwardG xs (ys, css) = (xs, chckValForG ys xs : css)

frontBackValG :: [Gear] -> [Gear]
frontBackValG [a] = [a]
frontBackValG ((Gear n):b:cs) = (Gear n) : (frontBackValG ((validateIfDigitG n b) : cs))
frontBackValG (a:(Gear n):cs) = (validateIfDigitG n a) : (frontBackValG ((Gear n) : cs))
frontBackValG (a:b:cs) = a : (frontBackValG (b : cs))

checkValidityG :: [[Gear]] -> [[Gear]]
checkValidityG = chckValDownFwdAndRev . chckValDownFwdAndRev . (fmap frontBackValG) where
    chckValDownFwdAndRev = reverse . snd . (foldr checkValidityForwardG (repeat NotGear, []))

runOnEach :: a -> [(a -> b)] -> [b]
runOnEach a [] = []
runOnEach a (f:fs) = f a : (runOnEach a fs)

mapIfValid :: Int -> [Int] -> [Gear] -> Int -> [Int]
mapIfValid n is [] i | elem i is = [n]
                     | otherwise = []
mapIfValid n js ((DigitG is k):cs) i = mapIfValid (10 * n + k) (is ++ js) cs i
mapIfValid n is (c:cs) i | elem i is = n : (mapIfValid 0 [] cs i)
                         | otherwise = mapIfValid 0 [] cs i

genGearMap :: [[Gear]] -> Int -> [Int]
genGearMap [] _ = []
genGearMap (gs:gss) n = (mapIfValid 0 [] gs n) ++ (genGearMap gss n)

validGears :: [Int] -> Int
validGears [a,b] = a * b
validGears _ = 0

combValidGears :: [[Int]] -> Int
combValidGears = sum . (fmap validGears)

stage2Test :: ParsedType -> [String]
stage2Test css = (fmap showGears) (checkValidityG gss) where
    (g, gss) = convAllCellsToGears 1 css

stage2 :: ParsedType -> Int
stage2 css = combValidGears (fmap (genGearMap (checkValidityG gss)) [1..g]) where
    (g, gss) = convAllCellsToGears 1 css

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parsedRow <* spaces) <* eof

parsedRow :: Parser [Cell]
parsedRow = some (parseCell)

parseCell :: Parser Cell
parseCell = (Digit False) <$> parseDigit
        <|> Space <$ char '.'
        <|> Symbol <$> noneOf ('.':'\r':'\n':' ':['0'..'9'])

parseDigit :: Parser Int
parseDigit = (read . (:[])) <$> (oneOf ['0'..'9'])