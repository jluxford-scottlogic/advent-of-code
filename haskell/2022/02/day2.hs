import Text.ParserCombinators.Parsec
import Control.Applicative (some)

data RPS = R | P | S
    deriving (Show, Eq)

type ParsedType = [(RPS, RPS)]

resolve :: (Integer, Integer, Integer) -> (RPS, RPS) -> Integer
resolve (win, draw, loss) (R, P) = win
resolve (win, draw, loss) (P, S) = win
resolve (win, draw, loss) (S, R) = win
resolve (win, draw, loss) (x, y) | x == y = draw
                                 | otherwise = loss

adjustment :: (Integer, Integer, Integer) -> RPS -> Integer
adjustment (rock, paper, scissors) R = rock
adjustment (rock, paper, scissors) P = paper
adjustment (rock, paper, scissors) S = scissors

stage1WDL = (6,3,0)
stage1RPS = (1,2,3)

calcScore :: [(RPS, RPS)] -> Integer
calcScore = sum . fmap (\(a,b) -> adjustment stage1RPS b + resolve stage1WDL (a,b))

calcScoreStage2 :: [(RPS, RPS)] -> Integer
calcScoreStage2 = calcScore . fmap convertStage2ToStage1

-- R = Lose, P = Draw, S = Win
convertStage2ToStage1 :: (RPS, RPS) -> (RPS, RPS)
convertStage2ToStage1 (x, P) = (x, x)
convertStage2ToStage1 (R, R) = (R, S)
convertStage2ToStage1 (P, R) = (P, R)
convertStage2ToStage1 (S, R) = (S, P)
convertStage2ToStage1 (R, S) = (R, P)
convertStage2ToStage1 (P, S) = (P, S)
convertStage2ToStage1 (S, S) = (S, R)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = parseTournament

parseTournament :: Parser [(RPS, RPS)]
parseTournament = some (parseRPSRound <* ((return () <* newline) <|> (return () <* eof)))

parseRPSRound :: Parser (RPS, RPS)
parseRPSRound = (,) <$> parseFP <* char ' ' <*> parseSP

parseFP :: Parser RPS
parseFP = R <$ char 'A'
      <|> P <$ char 'B'
      <|> S <$ char 'C'

parseSP :: Parser RPS
parseSP = R <$ char 'X'
      <|> P <$ char 'Y'
      <|> S <$ char 'Z'