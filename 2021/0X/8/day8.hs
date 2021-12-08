import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List ((\\), sort)

simpleDisplays :: [String] -> [String]
simpleDisplays = filter f where
    f :: String -> Bool
    f ns = length ns == 2 || length ns == 3 || length ns == 4 || length ns == 7

stage1 :: [([String], [String])] -> Int
stage1 = sum . fmap (length . simpleDisplays . snd)

-- 'a' = 8  (so get from display of 1 and 7)
-- 'b' =  6
-- 'c' = 8  (so get after 'a')
-- 'd' = 7  (so get from removal of display of 4)
-- 'e' =  4
-- 'f' =  9
-- 'g' = 7  (so get after 'd')

calcDisp :: [String] -> (Char -> Char)
calcDisp ns = upFun (upFun (upFun (upFun (upFun (upFun (upFun (const '.') b 'b') e 'e') a 'a') f 'f') c 'c') d 'd') g 'g' where
    nsWithoutFour = filter ((/= 4) . length) ns
    listOfCharByInt = foldl combChar [(0, 'a')] (sort (concat ns))
    combChar :: [(Int, Char)] -> Char -> [(Int, Char)]
    combChar ((n, c):cs) nc | c == nc = (n + 1, c) : cs
                            | otherwise = (1, nc) : (n, c) :cs
    b = snd $ head $ filter ((== 6) . fst) listOfCharByInt
    e = snd $ head $ filter ((== 4) . fst) listOfCharByInt
    f = snd $ head $ filter ((== 9) . fst) listOfCharByInt
    seven = head $ filter (\a -> 3 == length a) ns
    one = head $ filter (\a -> 2 == length a) ns
    a = head $ charDiff seven one
    c = snd $ head $ filter (\charInt -> (fst charInt == 8) && (snd charInt /= a)) listOfCharByInt
    listOfCharByIntWithoutFour = foldl combChar [(0, 'a')] (sort (concat nsWithoutFour))
    d = snd $ head $ filter ((== 6) . fst) listOfCharByIntWithoutFour
    g = snd $ head $ filter (\charInt -> (fst charInt == 7) && (snd charInt /= d)) listOfCharByInt

charDiff :: String -> String -> String
charDiff xs ys = foldl f xs ys where
    f :: String -> Char -> String
    f xs y | elem y xs = filter (/= y) xs
           | otherwise = xs

convertToValue :: (Char -> Char) -> String -> Int
convertToValue encoding value = valueConv (fmap encoding value)

upFun :: (Char -> Char) -> Char -> Char -> (Char -> Char)
upFun f a b c | a == c = b
              | otherwise = f c

valueConv :: [Char] -> Int
valueConv ['a','b','c','e','f','g'] = 0
valueConv ['c','f'] = 1
valueConv ['a','c','d','e','g'] = 2
valueConv ['a','c','d','f','g'] = 3
valueConv ['b','c','d','f'] = 4
valueConv ['a','b','d','f','g'] = 5
valueConv ['a','b','d','e','f','g'] = 6
valueConv ['a','c','f'] = 7
valueConv ['a','b','c','d','e','f','g'] = 8
valueConv ['a','b','c','d','f','g'] = 9
valueConv _ = undefined

encodeDec :: [Int] -> Int
encodeDec = foldl f 0 where
    f :: Int -> Int -> Int
    f a b = a * 10 + b

stage2 :: [([String], [String])] -> Int
stage2 = sum . fmap (\(all, digits) -> encodeDec (fmap (valueConv . sort . fmap (calcDisp all)) digits))

testStage2 :: [([String], [String])] -> [[Char]]
testStage2 = fmap (\(all, digits) -> fmap (calcDisp all) ['a'..'g'])

day8Parser :: Parser [([String], [String])]
day8Parser = some lineParser <* eof

lineParser :: Parser ([String], [String])
lineParser = (,) <$> some (dispParser <* many space) <* char '|' <*> some (dispParser <* many (char ' ')) <* many (char '\n')

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [([String], [String])])
parsedValue = parseFromFile day8Parser "day8input.txt"

parsedTest :: IO (Either ParseError [([String], [String])])
parsedTest = parseFromFile day8Parser "day8Testinput.txt"

dispParser :: Parser [Char]
dispParser = many (char ' ') *> some (oneOf ['a'..'g'])