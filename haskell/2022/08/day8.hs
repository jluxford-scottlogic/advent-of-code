import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [[Integer]]

appBoth :: (a -> a) -> (a, a) -> (a, a)
appBoth f (a,b) = (f a, f b)

splitForCon :: [[Integer]] -> [([Integer], [Integer])]
splitForCon = fmap halveList

halveList :: [Integer] -> ([Integer], [Integer])
halveList ks = (take midPoint ks, reverse (drop midPoint ks)) where
    midPoint = div (length ks) 2

addNegOneBorder :: [([Integer], [Integer])] -> [([Integer], [Integer])]
addNegOneBorder ks = (negOnes :) . reverse . (negOnes :) . reverse $ addLR where 
    addLR = fmap (appBoth ((-1):)) ks
    negOnes = halveList (take (length ks + 2) [-1,-1..])

findVisible :: [([Integer], [Integer])] -> Integer
findVisible (k:[]) = 0
findVisible (k:ks) = (zipWithPairs ) (init lowerVisible) ++  where
    ((lt,rt):lowerVisible) = findVisible (fmap (appBoth (chooseOnTrue (>=))) ks')
    ((lb,rb):_) = reverse (init lowerVisible)
    (topL, topR) = appBoth tail k
    (botL, botR) = appBoth tail (last ks)
    leftF = fmap (head . fst) ks
    rightF = fmap (head . snd) ks

fVisible :: [[(Integer, Integer)]] -> Integer
fVisible xss = where
    (t, b) = appBoth mid (droppedMid xss)
    l = mid (fmap head xss)
    r = mid (fmap last xss)
    c = mid (fmap mid xss)
    rows = zipWith (,,,) (zipWith max l r) (zipWith (,,) c l r)

adjustTB :: [(Integer, [((Integer, Integer), (Integer, Integer), (Integer, Integer))])] -> [(Integer, Integer)]
adjustTB ((m,xs):xss) = 

droppedMid :: [a] -> (a, a)
droppedMid (x:xs) = (x, last xs)

mid :: [a] -> [a]
mid [] = []
mid (x:xs) = init xs

chooseOnTrue :: (a -> a -> Bool) -> [a] -> [a]
chooseOnTrue (a:b:as) | f a b = a:as
                      | f b a = b:as
                      | otherwise = as
chooseOnTrue as = as

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some ((some parseDigit) <* (many newline)) <* eof

parseDigit :: Parser Integer
parseDigit = read . (:[]) <$> (oneOf ['0'..'9'])