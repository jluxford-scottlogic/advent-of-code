import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)

type ParsedType = ([Int], [[Map]])

--             Dst Src Dif
data Map = Map Int Int Int
    deriving (Show, Eq)

traceMaps :: [(Int -> Int)] -> [Int] -> [Int]
traceMaps [] ns = ns
traceMaps (f:fs) ns = traceMaps fs (fmap f ns)

unMapMaps :: [Map] -> Int -> Int
unMapMaps [] n = n
unMapMaps ((Map d s r):ms) n | n >= s && n < (s + r) = n - s + d
                             | otherwise = unMapMaps ms n

stage1Test :: ParsedType -> [Int]
stage1Test (i, (m:maps)) = (fmap ((unMapMaps) m) i)

stage1 :: ParsedType -> Int
stage1 (i, maps) = minimum (traceMaps (fmap (unMapMaps) maps) i)

type RangeList = [(Int, Int)]

updRange :: Map -> [(Bool, (Int, Int))] -> [(Bool, (Int, Int))]
updRange m [] = []
updRange m ((True, (l,u)):rs) = (True, (l,u)) : (updRange m rs)
updRange m@(Map d s r) ((False, (l,u)):rs) | s + r <= l = (False, (l, u)) : (updRange m rs)
                                           | u < s = (False, (l, u)) : (updRange m rs)
                                           | s + r >= u && s <= l = (True, (l - s + d, u - s + d)) : (updRange m rs)
                                           | s + r >= u = (True, (d, u - s + d)) : (updRange m ((False, (l, s - 1)) : rs))
                                           | s <= l = (True, (l - s + d, d + r - 1)) : (updRange m ((False, (s + r, u)) : rs))
                                           | otherwise = (True, (d, d + r - 1)) : (updRange m ((False, (l, s - 1)) : (False, (s + r, u)) : rs))

updRangeList :: [Map] -> [(Bool, (Int, Int))] -> [(Bool, (Int, Int))]
updRangeList [] xs = xs
updRangeList (m:ms) ks = updRangeList ms (updRange m ks)

updRangesLists :: [[Map]] -> [(Bool, (Int, Int))] -> [(Bool, (Int, Int))]
updRangesLists [] rs = rs
updRangesLists (ms:mss) rs = updRangesLists mss (updRangeList ms (fmap ((False,) . snd) rs))

intoRangeList :: [Int] -> [(Int, Int)]
intoRangeList [] = []
intoRangeList (x:r:xs) = (x, x + r - 1) : (intoRangeList xs)

stage2 :: ParsedType -> Int
stage2 (i, maps) = (minimum . (fmap (fst . snd)) . (updRangesLists maps) . (fmap (False,))) ranges where
    ranges = intoRangeList i

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (,) <$> parseSeeds <* spaces <*> (some (parseMaps <* spaces)) <* eof

parseSeeds :: Parser [Int]
parseSeeds = (string "seeds:") *> (some ( string " " *> parseNum))

parseMaps :: Parser [Map]
parseMaps = (some (oneOf ('-':' ':':':['a'..'z'])) <* spaces) *> some (parseMap) <* spaces

parseMap :: Parser Map
parseMap = Map <$> parseNum <* string " " <*> parseNum <* string " " <*> parseNum <* spaces

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))