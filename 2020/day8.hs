import Text.ParserCombinators.Parsec
import Control.Applicative (some)

data Instruction = Acc Int | Jmp Int | Nop Int
    deriving (Show, Eq)

runInstructionsOrder :: [Maybe Instruction] -> [Maybe Instruction] -> Int -> [Instruction]
runInstructionsOrder as (Nothing:bs) acc = []
runInstructionsOrder as (Just (Acc n) : bs) acc = Acc n : runInstructionsOrder (Nothing : as) bs (acc + n)
runInstructionsOrder as (Just (Nop n) : bs) acc = Nop n : runInstructionsOrder (Nothing : as) bs acc
runInstructionsOrder as (Just (Jmp n) : bs) acc | n == 0 = [Jmp n]
                                                | n < 0  = Jmp n : runInstructionsOrder (drop (n * (-1)) as) (reverse (take (n * (-1)) as) ++ (Nothing : bs)) acc
                                                | otherwise = Jmp n : runInstructionsOrder (reverse (take (n - 1) bs) ++ (Nothing:as)) (drop (n - 1) bs) acc

terminates :: [Maybe Instruction] -> [Maybe Instruction] -> Int -> (Bool, Int)
terminates as [] acc = (True, acc)
terminates as (Nothing:bs) acc = (False, acc)
terminates as (Just (Acc n) : bs) acc = terminates (Nothing : as) bs (acc + n)
terminates as (Just (Nop n) : bs) acc = terminates (Nothing : as) bs acc
terminates as (Just (Jmp n) : bs) acc | n == 0 = (False, acc)
                                      | n < 0  = terminates (drop (n * (-1)) as) (reverse (take (n * (-1)) as) ++ (Nothing : bs)) acc
                                      | otherwise = terminates (reverse (take (n - 1) bs) ++ (Nothing:as)) (drop (n - 1) bs) acc

genAllSwaps :: [Maybe Instruction] -> [[Maybe Instruction]]
genAllSwaps = fmap reverse . snd . foldl f ([],[]) where
    f :: ([Maybe Instruction], [[Maybe Instruction]]) -> Maybe Instruction -> ([Maybe Instruction], [[Maybe Instruction]])
    f (prevAcc, adjusted) Nothing = (Nothing : prevAcc, fmap (Nothing : ) adjusted)
    f (prevAcc, adjusted) i@(Just (Acc n)) = (i : prevAcc, fmap (i : ) adjusted)
    f (prevAcc, adjusted) i@(Just (Jmp n)) = (i : prevAcc, (Just (Nop n) : prevAcc) : fmap (i : ) adjusted)
    f (prevAcc, adjusted) i@(Just (Nop n)) = (i : prevAcc, (Just (Jmp n) : prevAcc) : fmap (i : ) adjusted)

testInfiniteLoop :: [Instruction] -> Int
testInfiniteLoop is = snd $ terminates [] (fmap Just is) 0

testInfiniteLoopOrder :: [Instruction] -> [Instruction]
testInfiniteLoopOrder is = runInstructionsOrder [] (fmap Just is) 0

testAllSwaps :: [Instruction] -> Int
testAllSwaps is = snd $ head $ filter fst $ fmap (\as -> terminates [] as 0) (genAllSwaps (fmap Just is))

inputParser :: Parser [Instruction]
inputParser = some instructionParser <* eof

instructionParser :: Parser Instruction
instructionParser = Acc <$ string "acc " <*> signedIntParser <* many space
                <|> Jmp <$ string "jmp " <*> signedIntParser <* many space
                <|> Nop <$ string "nop " <*> signedIntParser <* many space

signedIntParser :: Parser Int
signedIntParser = ((*) (-1) . read) <$ oneOf "-" <*> some digit
              <|> read <$ oneOf "+" <*> some digit

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Instruction])
parsedValue = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day8Input.txt"

parsedTest :: IO (Either ParseError [Instruction])
parsedTest = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day8TestInput.txt"

parsedTest2 :: IO (Either ParseError [Instruction])
parsedTest2 = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day8TestInput2.txt"