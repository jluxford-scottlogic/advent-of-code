import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import Data.Map.Strict

type ParsedType = [MonkeyLong]

data MonkeyLong = MonkeyLong Int [Int] Expr Int Int Int
    deriving (Show, Eq)

data Expr = Mult Val Val | Add Val Val
    deriving (Show, Eq)
data Val = X | Num Int
    deriving (Show, Eq)

--            Monkey id executions objs updFunc testFunc   throwOnTrue throwOnFalse
data Monkey = Monkey Int Int [Int] (Int -> Int) (Int -> Bool) Int Int

instance Show Monkey where
    show (Monkey id n ns _ _ _ _) = "Monkey " ++ show id ++ " " ++ show n ++ " " ++ show ns

evalExpr :: Expr -> Int -> Int
evalExpr (Mult v w) x = evalVal v x * evalVal w x
evalExpr (Add v w) x = evalVal v x + evalVal w x

evalVal :: Val -> Int -> Int
evalVal X n = n
evalVal (Num m) _ = m

evalCondition :: Int -> Int -> Bool
evalCondition divCon n = mod n divCon == 0

deWorry :: (Int -> Int) -> Int -> Int
deWorry f n = div (f n) 3

convertMonkeyList :: [MonkeyLong] -> Map Int Monkey
convertMonkeyList [] = empty
convertMonkeyList ((MonkeyLong id worries expr divCon monkT monkF):ms) = insert id (Monkey id 0 worries (deWorry (evalExpr expr)) (evalCondition divCon) monkT monkF) (convertMonkeyList ms)

calculateTurn :: Int -> Map Int Monkey -> Map Int Monkey
calculateTurn monkId map = mapMoveIsF where
    (Monkey id n is f g mt mf) = map ! monkId
    mapAdjThisMonkey = insert monkId (Monkey id (n + (length is)) [] f g mt mf) map
    newVals = fmap f is
    isT = Prelude.filter g newVals
    isF = Prelude.filter (not . g) newVals
    mapMoveIsT = insert mt (addToGivenMonkey isT (mapAdjThisMonkey ! mt)) mapAdjThisMonkey
    mapMoveIsF = insert mf (addToGivenMonkey isF (mapMoveIsT ! mf)) mapMoveIsT

addToGivenMonkey :: [Int] -> Monkey -> Monkey
addToGivenMonkey ns (Monkey id n is f g mt mf) = (Monkey id n (is ++ ns) f g mt mf)

takeRound :: [Int] -> Map Int Monkey -> Map Int Monkey
takeRound [] map = map
takeRound (n:ns) map = takeRound ns (calculateTurn n map)

calculateRound :: Map Int Monkey -> Map Int Monkey
calculateRound map = takeRound [0..(size map) - 1] map

stepRound :: Int -> Map Int Monkey -> Map Int Monkey
stepRound n map | n == 0 = map
                | otherwise = stepRound (n - 1) (calculateRound map)

stage1Round :: Int -> [MonkeyLong] -> Map Int Monkey
stage1Round n = (stepRound n) . convertMonkeyList

extractExecutions :: Monkey -> Int
extractExecutions (Monkey _ n _ _ _ _ _) = n

topTwoMult :: [Int] -> Int
topTwoMult ns = a * b where
    (a, b) = Prelude.foldr maxBetween (0,0) ns
    maxBetween :: Int -> (Int, Int) -> (Int, Int)
    maxBetween n (c, d) | d > n = (c, d)
                        | c > n = (c, n)
                        | otherwise = (n, c)

stage1 :: [MonkeyLong] -> Int
stage1 = topTwoMult . (foldr' ((:) . extractExecutions) []) . (stepRound 20) . convertMonkeyList

modByTotal :: Int -> (Int -> Int) -> Int -> Int
modByTotal m f n = mod (f n) m

convertMonkeyList2 :: [MonkeyLong] -> Map Int Monkey
convertMonkeyList2 [] = empty
convertMonkeyList2 ms = convertMonkeyList2Helper maxMod ms where
    maxMod = Prelude.foldr f 1 ms
    f (MonkeyLong _ _ _ modVal _ _) n = modVal * n

convertMonkeyList2Helper :: Int -> [MonkeyLong] -> Map Int Monkey
convertMonkeyList2Helper maxMod [] = empty
convertMonkeyList2Helper maxMod ((MonkeyLong id worries expr divCon monkT monkF):ms) = insert id (Monkey id 0 worries (modByTotal maxMod (evalExpr expr)) (evalCondition divCon) monkT monkF) (convertMonkeyList2Helper maxMod ms)

stage2Test :: Int -> [MonkeyLong] -> [Int]
stage2Test n = (foldr' ((:) . extractExecutions) []) . (stepRound n) . convertMonkeyList2

stage2 :: [MonkeyLong] -> Int
stage2 = topTwoMult . (foldr' ((:) . extractExecutions) []) . (stepRound 10000) . convertMonkeyList2

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
usedParser = some (parseMonkeyLong <* many space) <* eof

parseMonkeyLong :: Parser MonkeyLong
parseMonkeyLong = MonkeyLong <$ string "Monkey " <*> parseNum <* string ":"
               <* many space <* string "Starting items: " <*> some (parseNum <* many (oneOf [',',' ']))
               <* many space <* string "Operation: new = " <*> parseExpr
               <* many space <* string "Test: divisible by " <*> parseNum
               <* many space <* string "If true: throw to monkey " <*> parseNum
               <* many space <* string "If false: throw to monkey " <*> parseNum

parseExpr :: Parser Expr
parseExpr = parseVal >>= parseExprWithVal

parseExprWithVal :: Val -> Parser Expr
parseExprWithVal v = Mult <$ many space <* string "* " <*> return v <*> parseVal
                 <|> Add <$ many space <* string "+ " <*> return v <*> parseVal

parseVal :: Parser Val
parseVal = X <$ string "old" <* many space
       <|> Num <$> parseNum <* many space

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ['0'..'9']))