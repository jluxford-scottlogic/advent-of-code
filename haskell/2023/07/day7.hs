import Text.Parsec.ByteString
import Text.Parsec
import Control.Applicative (some)
import Data.List (sort)

type ParsedType = [Hand]

data Hand = Hand Int [Int] Int
    deriving (Show)

instance Eq Hand where
    (Hand _ as _) == (Hand _ bs _) = as == bs

instance Ord Hand where
    (Hand i as _) <= (Hand j bs _) = rank i as < rank j bs || (rank i as == rank j bs && lowerFirst as bs) || as == bs

rank :: Int -> [Int] -> Int
rank i as = rankPS i (sort as)

rankPS :: Int -> [Int] -> Int
rankPS i as | ofAKind (5 - i) noJks = 7
            | ofAKind (4 - i) noJks = 6
            | fullHouse i     noJks = 5
            | ofAKind (3 - i) noJks = 4
            | twoPair i       noJks = 3
            | ofAKind (2 - i) noJks = 2
            | otherwise = 1 where
                noJks = rmvJokers as

ofAKind :: Int -> [Int] -> Bool
ofAKind 0 _ = True
ofAKind n [] = False
ofAKind n (a:as) = (length (a:as) >= n && allSame (take n (a:as))) || ofAKind n as

allSame :: [Int] -> Bool
allSame [] = True
allSame [b] = True
allSame (a:b:as) = a == b && allSame (b:as)

rmvJokers :: [Int] -> [Int]
rmvJokers [] = []
rmvJokers (1:as) = rmvJokers as
rmvJokers (a:as) = a:as

fullHouse :: Int -> [Int] -> Bool
fullHouse 3 as = True
fullHouse 2 as = ofAKind 2 as
fullHouse 1 as = twoPair 0 as || ofAKind 3 as
fullHouse 0 (a:b:c:d:e:[]) = a == b && c == d && d == e || a == b && b == c && d == e

twoPair :: Int -> [Int] -> Bool
twoPair 2 as = True
twoPair 1 as = ofAKind 2 as
twoPair 0 [] = False
twoPair 0 [a] = False
twoPair 0 [a,b,c] = False
twoPair 0 (a:b:as) = (a == b && ofAKind 2 as) || twoPair 0 (b:as)

lowerFirst :: [Int] -> [Int] -> Bool
lowerFirst [] [] = True
lowerFirst (a:as) (b:bs) = a < b || (a == b && lowerFirst as bs)

sumAndInc :: (Int, Int) -> Hand -> (Int, Int)
sumAndInc (n, s) (Hand _ _ bid) = (n + 1, s + (n * bid))

stage1Test :: ParsedType -> ParsedType
stage1Test = sort

stage1 :: ParsedType -> Int
stage1 = snd . (foldl sumAndInc (1,0)) . sort

convJacksToJokers :: Hand -> Hand
convJacksToJokers (Hand j [] bid) = (Hand j [] bid)
convJacksToJokers (Hand j (11:as) bid) = addCard 1 (convJacksToJokers (Hand (j + 1) as bid))
convJacksToJokers (Hand j (a:as) bid) = addCard a (convJacksToJokers (Hand j as bid))

addCard :: Int -> Hand -> Hand
addCard a (Hand j as bid) = Hand j (a:as) bid

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
parseHand = (Hand 0) <$> (some parseCard) <* space <*> parseNum

parseCard :: Parser Int
parseCard = parseDigit
        <|> 10 <$ char 'T'
        <|> 11 <$ char 'J'
        <|> 12 <$ char 'Q'
        <|> 13 <$ char 'K'
        <|> 14 <$ char 'A'

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))

parseDigit :: Parser Int
parseDigit = (read . (:[])) <$> (oneOf ['0'..'9'])