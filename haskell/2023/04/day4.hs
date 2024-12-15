import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [([Int], [Int])]

contains :: Eq a => a -> [a] -> Bool
contains a [] = False
contains a (b:bs) = a == b || contains a bs

subsetSize :: (Int -> Int) -> ([Int], [Int]) -> Int
subsetSize f ([],_) = 0
subsetSize f (x:xs,ys) | contains x ys = max (f (subsetSize f (xs, ys))) 1
                     | otherwise = subsetSize f (xs, ys)

stage1 :: ParsedType -> Int
stage1 = sum . (fmap (subsetSize (2*)))

incEachUp :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
incEachUp _ 0 ns = ns
incEachUp s k ((a,b):ns) = (a + s, b) : (incEachUp s (k - 1) ns)

incRepetatively :: [(Int, Int)] -> [Int]
incRepetatively [] = []
incRepetatively ((a, 0):ns) = a : incRepetatively ns
incRepetatively ((a, b):ns) = a : (incRepetatively (incEachUp a b ns))

stage2Test :: ParsedType -> [(Int, Int)]
stage2Test = (fmap (1,)) . (fmap (subsetSize (1+)))

stage2 :: ParsedType -> Int
stage2 = sum . incRepetatively . (fmap (1,)) . (fmap (subsetSize (1+)))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some parseCard <* eof

parseCard :: Parser ([Int], [Int])
parseCard = (,) <$ (string "Card " <* spaces <* parseNum <* string ":" <* spaces) <*> (parseNumList) <* string "|" <*> parseNumList

parseNumList :: Parser [Int]
parseNumList = spaces *> (some (parseNum <* spaces))

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))