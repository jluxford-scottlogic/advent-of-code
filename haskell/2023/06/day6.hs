import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = ([Int], [Int])

createValues :: Int -> [Int]
createValues n = [(n - x) * x | x <- [0..n]]

compareValues :: ([[Int]], [Int]) -> [Int]
compareValues ([],[]) = []
compareValues (xs:xss, y:ys) = length (filter (> y) xs) : compareValues (xss, ys)

stage1 :: ParsedType -> Int
stage1 (xs, ys) = product (compareValues (fmap createValues xs, ys))

combValues :: [Int] -> Int
combValues ns = read (concat (fmap show ns))

stage2 :: ParsedType -> Int
stage2 (xs, ys) = length (filter (> (combValues ys)) (createValues (combValues xs)))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (,) <$ string "Time: " <* spaces <*> parseNumList <* spaces <* string "Distance: " <* spaces <*> parseNumList <* spaces <* eof

parseNumList :: Parser [Int]
parseNumList = some (parseNum <* spaces)

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ('-':['0'..'9'])))