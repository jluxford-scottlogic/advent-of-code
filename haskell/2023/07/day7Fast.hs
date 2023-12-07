import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import Data.List (sort)

type ParsedType = [Hand]

data Hand = Hand Int Int [Int] Int
    deriving (Show, Eq)

instance Ord Hand where
    (Hand ra _ va _) <= (Hand rb _ vb _) = ra < rb || (ra == rb && va < vb) || va == vb

rank :: Int -> [Int] -> Int
rank i as = rankPS i (rmvJokers (sort as))

rankPS :: Int -> [Int] -> Int
rankPS j as | kv > 4 = 7
            | kv > 3 = 6
            | fullHouse j as = 5
            | kv > 2 = 4
            | twoPair j as = 3
            | kv > 1 = 2
            | otherwise = 1 where
                kv = j + kindValue as

kindValue :: [Int] -> Int
kindValue = fst . (foldr kindValueF (1,(0,0)))

kindValueF :: Int -> (Int, (Int, Int)) -> (Int, (Int, Int))
kindValueF k (mx, (n, c)) | k == n && mx <= c = (c + 1, (n, c + 1))
                          | k == n = (mx, (n, c + 1))
                          | otherwise = (mx, (k, 1))

rmvJokers :: [Int] -> [Int]
rmvJokers [] = []
rmvJokers (1:as) = rmvJokers as
rmvJokers (a:as) = a:as

fullHouse :: Int -> [Int] -> Bool
fullHouse 2 as = kindValue as > 1
fullHouse 1 as = twoPair 0 as || kindValue as > 2
fullHouse 0 (a:b:c:d:e:[]) = a == b && c == d && d == e || a == b && b == c && d == e

twoPair :: Int -> [Int] -> Bool
twoPair 0 (a:b:as) = (a == b && (kindValue as > 1)) || twoPair 0 (b:as)
twoPair _ _ = False

cacheRankAndValue :: Hand -> Hand
cacheRankAndValue (Hand _ j as bid) = Hand (rank j as) j as bid

sumAndInc :: (Int, Int) -> Hand -> (Int, Int)
sumAndInc (n, s) (Hand _ _ _ bid) = (n + 1, s + (n * bid))

stage1 :: ParsedType -> Int
stage1 = snd . (foldl sumAndInc (1,0)) . sort . (fmap cacheRankAndValue)

convJacksToJokers :: Hand -> Hand
convJacksToJokers (Hand r j [] bid) = (Hand r j [] bid)
convJacksToJokers (Hand r j (11:as) bid) = addCard 1 (convJacksToJokers (Hand r (j + 1) as bid))
convJacksToJokers (Hand r j (a:as) bid) = addCard a (convJacksToJokers (Hand r j as bid))

addCard :: Int -> Hand -> Hand
addCard a (Hand r j as bid) = Hand r j (a:as) bid

stage2 :: ParsedType -> Int
stage2 = stage1 . (fmap convJacksToJokers)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseHand <* spaces) <* eof

parseHand :: Parser Hand
parseHand = (Hand 0 0) <$> (some parseCard) <* space <*> parseNum

parseCard :: Parser Int
parseCard = parseDigit
        <|> 10 <$ char 'T'
        <|> 11 <$ char 'J'
        <|> 12 <$ char 'Q'
        <|> 13 <$ char 'K'
        <|> 14 <$ char 'A'

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ['0'..'9']))

parseDigit :: Parser Int
parseDigit = (read . (:[])) <$> (oneOf ['0'..'9'])