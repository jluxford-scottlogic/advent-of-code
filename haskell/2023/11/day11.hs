import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (transpose)

type ParsedType = [[Maybe (Either Int Int)]]

allEq :: Eq a => (a -> Bool) -> [a] -> Bool
allEq _ [] = True
allEq f (a:as) = f a && allEq f as

xNothings :: Int -> [[Maybe (Either Int Int)]] -> [[Maybe (Either Int Int)]]
xNothings n [] = []
xNothings n (xs:xss) | allEq (\a -> a == Nothing || a == Just (Right n)) xs = (fmap (\_ -> Just (Right n)) xs):(xNothings n xss)
                     | otherwise = xs : (xNothings n xss)

xNothingsRowAndColumn :: Int -> [[Maybe (Either Int Int)]] -> [[Maybe (Either Int Int)]]
xNothingsRowAndColumn n = transpose . (xNothings n) . transpose . (xNothings n)

locations :: (Int, Int) -> [[Maybe (Either Int Int)]] -> [(Int, Int)]
locations _ [] = []
locations (x,y) ([]:((Just (Right n)):gs):gss) | allEq (== Just (Right n)) gs = locations (0, y + n) ([]:gss)
                                               | otherwise = locations (0, y + 1) (((Just (Right n)):gs):gss)
locations (x,y) ([]:gss) = locations (0, y + 1) gss
locations (x,y) ((Nothing:gs):gss) = locations (x + 1, y) (gs:gss)
locations (x,y) (((Just (Left _)):gs):gss) = (x,y) : (locations (x + 1, y) (gs:gss))
locations (x,y) (((Just (Right n)):gs):gss) = (locations (x + n, y) (gs:gss))

findDiffs :: (Int, Int) -> [(Int, Int)] -> Int
findDiffs _ [] = 0
findDiffs (x,y) ((a,b):as) = abs (x - a) + abs (y - b) + findDiffs (x,y) as

findAllDiffs :: [(Int, Int)] -> Int
findAllDiffs [] = 0
findAllDiffs (xs:xss) = findDiffs xs xss + findAllDiffs xss

stage1 :: ParsedType -> Int
stage1 = findAllDiffs . (locations (0,0)) . (xNothingsRowAndColumn 2)

stageN :: Int -> ParsedType -> Int
stageN n = findAllDiffs . (locations (0,0)) . (xNothingsRowAndColumn n)

stage2 :: ParsedType -> Int
stage2 = findAllDiffs . (locations (0,0)) . (xNothingsRowAndColumn 1000000)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

someWithInt :: (a -> Parser (a, b)) -> (a, [b]) -> Parser (a, [b])
someWithInt p (a, bs) = try (p a >>= (\(a', b) -> someWithInt p (a', b:bs))) 
                    <|> try (p a >>= (\(a', b) -> return (a', b:bs)))

usedParser :: Parser ParsedType
usedParser = ((fmap reverse) . reverse . snd) <$> (someWithInt (\n -> ((parseLine n) <* spaces)) (1, [])) <* eof

parseLine :: Int -> Parser (Int, [Maybe (Either Int Int)])
parseLine n = someWithInt parseCell (n, [])

parseCell :: Int -> Parser (Int, Maybe (Either Int Int))
parseCell n = (n, Nothing) <$ char '.' <|> (n + 1, Just (Left n)) <$ char '#'