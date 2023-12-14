import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import Data.Map.Strict
import Data.List (elemIndex)

type ParsedType = ([RL], [(String, (String, String))])

data RL = R | L
    deriving (Show, Eq, Ord)

makeMap :: (Map (String, RL) String) -> RL -> String -> String
makeMap map rl k = map ! (k, rl)

interMap :: [(String, (String, String))] -> Map (String, RL) String
interMap [] = empty
interMap ((s, (l,r)):ss) = insert (s,R) r (insert (s,L) l (interMap ss))

stepsUntil :: (String -> Bool) -> (RL -> String -> String) -> String -> [RL] -> Integer
stepsUntil end f pos (rl:dirs) | end pos = 0
                               | otherwise = 1 + stepsUntil end f (f rl pos) dirs

stage1 :: ParsedType -> Integer
stage1 (dirs, map) = stepsUntil (=="ZZZ") newMap "AAA" (concat (repeat dirs)) where
    newMap = makeMap (interMap map)

buildUntil :: (String -> Bool) -> (RL -> String -> String) -> [[RL]] -> [(String, (Bool, [RL]))] -> [(String, (Bool, [RL]))]
buildUntil end f ((rl:[]):dirs:dss) ((p,b):ps) | elem (p, b) ps = (p,b):ps
                                               | otherwise = buildUntil end f (dirs:dss) (((f rl p), (end (f rl p), dirs)):(p,b):ps)
buildUntil end f ((rl:dirs):dss) ((p,b):ps) | elem (p, b) ps = (p,b):ps
                                            | otherwise = buildUntil end f (dirs:dss) (((f rl p), (end (f rl p), dirs)):(p,b):ps)

convertCyclesToIndex :: [(String, (Bool,[RL]))] -> [(Int, (Bool, [RL]))]
convertCyclesToIndex ps = reverse (fmap (\(i, b) -> (\(Just a) -> (a, b)) (elemIndex (i,b) ps)) ps)

infCycleHelper :: Int -> [(Int, (Bool, [RL]))] -> [(Int -> Int)]
infCycleHelper n [] = [(+1)]
infCycleHelper n [(a,_)] = [(+(n + a))]
infCycleHelper n ((a,_):(b,_):cs) = (+(a - b)) : (infCycleHelper n cs)

addTillTrue :: [(Int, (Bool, [RL]))] -> Int -> Int
addTillTrue ((_,(False,_)):cs) = (+1) . (addTillTrue cs)
addTillTrue ((_,(True,_)):_) = (+1)

infCycle :: [(Int, (Bool, [RL]))] -> [(Int -> Int)]
infCycle ((0,(_,_)):(n,(b,rls)):cs) = addTillTrue ((n,(b,rls)):cs) : (concat (repeat vals)) where
    justTrues = (Prelude.filter (\(_,(c,_)) -> c) ((n,(b,rls)):cs))
    vals = (infCycleHelper (n + 1 - (fst (head justTrues))) justTrues)
infCycle ((_,(_,_)):cs) = ((+1) . f) : fs where 
    f:fs = (infCycle cs)

allTrue :: [[Bool]] -> Int
allTrue bss | and (fmap head bss) = 0
            | otherwise = (+1) $! (allTrue (fmap tail bss))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

left :: Either a b -> a
left (Left a) = a

right :: Either a b -> b
right (Right b) = b

repeatZipsTillEq :: [([(Int -> Int)], Int)] -> Int
repeatZipsTillEq ((fs,n):ns) | isLeft value = left value
                             | otherwise =  repeatZipsTillEq (right value)  where
    value = (repeatZipsTillEqHelper (fs,n) ns)
    repeatZipsTillEqHelper :: ([(Int -> Int)], Int) -> [([(Int -> Int)], Int)] -> Either Int [([(Int -> Int)], Int)]
    repeatZipsTillEqHelper ((f:fs),n) (((g:gs),m):[]) | m == n = Left n
                                                      | m < n = Right [((f:fs), n), (gs, g m)]
                                                      | otherwise = Right [((g:gs),m), (fs, f n)]
    repeatZipsTillEqHelper (fs,n) ((gs,m):ms) | m == n = fmap ((gs,m):) (repeatZipsTillEqHelper (fs,n) ms)
                                              | m < n = fmap ((fs,n):) (repeatZipsTillEqHelper2 (gs,m) ms)
                                              | otherwise = fmap ((gs,m):) (repeatZipsTillEqHelper2 (fs,n) ms)
    repeatZipsTillEqHelper2 :: ([(Int -> Int)], Int) -> [([(Int -> Int)], Int)] -> Either Int [([(Int -> Int)], Int)]
    repeatZipsTillEqHelper2 ((f:fs),n) (((g:gs),m):[]) | m < n = Right [((f:fs), n), (gs, g m)]
                                                       | otherwise = Right [((g:gs),m), (fs, f n)]
    repeatZipsTillEqHelper2 (fs,n) ((gs,m):ms) | m < n = fmap ((fs,n):) (repeatZipsTillEqHelper2 (gs,m) ms)
                                               | otherwise = fmap ((gs,m):) (repeatZipsTillEqHelper2 (fs,n) ms)

stage2 :: ParsedType -> Int
stage2 (dirs, map) = repeatZipsTillEq zippedWithZeros where
    fullCycles = fmap (\a -> buildUntil (ends 'Z') newMap (repeat dirs) [(a, (False, dirs))]) allAs
    is = fmap convertCyclesToIndex fullCycles
    infIs = fmap infCycle is
    zippedWithZeros = fmap (\(f:fs,a) -> (fs, f a)) (zip infIs (repeat 0))
    newMap = makeMap (interMap map)
    ends :: Char -> String -> Bool
    ends c [_,_,d] = c == d
    ends _ _ = False
    allAs = Prelude.filter (ends 'A') (fmap fst map)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedTest2 :: IO (Either ParseError ParsedType)
parsedTest2 = parseFromFile usedParser "test2.txt"

parsedTest3 :: IO (Either ParseError ParsedType)
parsedTest3 = parseFromFile usedParser "test3.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (,) <$> (some parseRL) <* spaces <*> (some (parseRoute <* spaces)) <* eof

parseRL :: Parser RL
parseRL = R <$ char 'R' <|> L <$ char 'L'

parseRoute :: Parser (String, (String, String))
parseRoute = (,) <$> parseNode <* space <* char '=' <* space <*> parseNodePair

parseNodePair :: Parser (String, String)
parseNodePair = (,) <$> (char '(' *> parseNode) <* string ", " <*> parseNode <* char ')'

parseNode :: Parser String
parseNode = (some (oneOf (['A'..'Z'] ++ ['0'..'9'])))