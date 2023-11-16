
import Text.ParserCombinators.Parsec
import Control.Applicative (some)

slopeOptions :: [(Int, Int)]
slopeOptions = [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]

multValue :: [(Int, Int)] -> [[Int]] -> Int
multValue f slope = product (fmap (`treesHit` slope) f)

treesHit :: (Int , Int) -> [[Int]] -> Int
treesHit  _ [] = 0
treesHit (x, y) ((c:cs):css) = c + treesHit (x, y) (drop y (fmap (drop x) ((c:cs):css)))

day3Parser :: Parser [[Int]]
day3Parser = some day3SubParser <* eof

day3SubParser :: Parser [Int]
day3SubParser = fmap (concat . repeat) (some dotOrHash) <* maybeNewLine where
    dotOrHash :: Parser Int
    dotOrHash = 0 <$ char '.'
        <|> 1 <$ char '#'
    maybeNewLine :: Parser Char
    maybeNewLine = char '\n' <|> char ' '

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [[Int]])
parsedValue = parseFromFile day3Parser "C:/Dev/advent-of-code/2020/day3Input.txt"