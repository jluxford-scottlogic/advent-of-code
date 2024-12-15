import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [Instruction]

data Instruction = Equals String Integer | Dash String
    deriving (Eq, Show)

charAsciiConv :: Char -> Integer
charAsciiConv n = findIn n ((zip ['a'..'z'] [97..122]) ++ zip ['A'..'Z'] [65..90])

findIn :: Eq a => a -> [(a, b)] -> b
findIn a ((k,v):ks) | a == k = v
                    | otherwise = findIn a ks

intAsciiConv :: Integer -> Integer
intAsciiConv n = n + 48

intHash :: Integer -> Integer -> Integer
intHash init v = hashWith init intAsciiConv v

strHash :: Integer -> [Char] -> Integer
strHash = foldl (\b -> hashWith b charAsciiConv)

hashWith :: Integer -> (a -> Integer) -> a -> Integer
hashWith i f v = mod ((i + f v) * 17) 256

hashVal :: Integer -> Integer -> Integer
hashVal i v = mod ((i + v) * 17) 256

instrHash :: Instruction -> Integer
instrHash (Equals s i) = intHash (hashVal (strHash 0 s) 61) i
instrHash (Dash s) = hashVal (strHash 0 s) 45

stage1Test :: [Instruction] -> [Integer]
stage1Test = fmap instrHash

stage1 :: [Instruction] -> Integer
stage1 = sum . stage1Test

type HashMap = Integer -> [(String, Integer)]

emptyHashMap :: HashMap
emptyHashMap _ = []

hasLabel :: HashMap -> Integer -> String -> Bool
hasLabel hm n s = hasIn s (hm n)

hasIn :: Eq a => a -> [(a, b)] -> Bool
hasIn _ [] = False
hasIn a ((x,y):xs) = a == x || hasIn a xs

addToHashMap :: HashMap -> Integer -> (String, Integer) -> HashMap
addToHashMap hm n i k | n == k = reverse (i : (reverse (hm n)))
                      | otherwise = hm k

replaceInHashMap :: HashMap -> Integer -> (String, Integer) -> HashMap
replaceInHashMap hm n (s,f) k | n == k = replaceIn f s (hm n)
                              | otherwise = hm k

replaceIn :: Eq a => b -> a -> [(a,b)] -> [(a,b)]
replaceIn f s ((x,y):xs) | s == x = (s,f):xs
                         | otherwise = (x,y) : (replaceIn f s xs)

rmvInHashMap :: HashMap -> Integer -> String -> HashMap
rmvInHashMap hm n i k | n == k = rmvFst i (hm n)
                      | otherwise = hm k

rmvFst :: Eq a => a -> [(a,b)] -> [(a,b)]
rmvFst a [] = []
rmvFst a ((x,y):xs) | a == x = xs
                    | otherwise = (x,y) : (rmvFst a xs)

adjHashMap :: Instruction -> HashMap -> HashMap
adjHashMap (Equals s n) hm | hasLabel hm hash s = replaceInHashMap hm hash (s, n)
                           | otherwise = addToHashMap hm hash (s, n) where
                                hash = (strHash 0 s)
adjHashMap (Dash s) hm = rmvInHashMap hm (strHash 0 s) s

makeHashMap :: [Instruction] -> HashMap
makeHashMap = foldl (flip adjHashMap) emptyHashMap

stage2Test :: Integer -> [Instruction] -> [(String, Integer)]
stage2Test n is = (makeHashMap is) n

evaluateMap :: HashMap -> Integer
evaluateMap hm = sum (map (evaluateVal hm) [0..255])

evaluateVal :: HashMap -> Integer -> Integer
evaluateVal hm v = sum (map combineDown vals) where
    vals = zip [1..] (hm v)
    combineDown :: (Integer, (String, Integer)) -> Integer
    combineDown (n,(_,i)) = i * n * (v + 1)

stage2 :: [Instruction] -> Integer
stage2 is = evaluateMap (makeHashMap is)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseInstruction <* ((return () <* string ",") <|> eof))

parseInstruction :: Parser Instruction
parseInstruction = try (Equals <$> parseStr <* string "=" <*> parseNum)
               <|> Dash <$> parseStr <* string "-"

parseStr :: Parser String
parseStr = some (oneOf ['a'..'z'])

parseNum :: Parser Integer
parseNum = (read . (:[])) <$> (oneOf ['0'..'9'])