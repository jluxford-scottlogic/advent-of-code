import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type ParsedType = (Map Int [Char], [(Int, Int, Int)])

data TernChar = EOC | EOL | E | C Char

stage1 :: ParsedType -> String
stage1 (map, []) = grabFirsts map
stage1 (map, m:ms) = stage1 ((executeMove reverse map m), ms)

stage2 :: ParsedType -> String
stage2 (map, []) = grabFirsts map
stage2 (map, m:ms) = stage2 ((executeMove id map m), ms)

grabFirsts :: Map Int [Char] -> [Char]
grabFirsts map = [head (Map.findWithDefault [' '] cs map) | cs <- take (length (Map.keys map)) [1..]]

executeMove :: (String -> String) -> Map Int [Char] -> (Int, Int, Int) -> Map Int [Char]
executeMove f map (numberToMove, columnFrom, columnTo) = Map.insert columnTo newTo (Map.insert columnFrom newFrom map) where
    currentTo = (Map.findWithDefault [] columnTo map)
    currentFrom = (Map.findWithDefault [] columnFrom map)
    newTo = f (take numberToMove currentFrom) ++ currentTo
    newFrom = drop numberToMove currentFrom

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = (,) <$> (parseCrates 1 Map.empty) <* parseColumnNumerals <* (many newline) <*> (some parseMove) <* eof

parseCrates :: Int -> (Map Int [Char]) -> Parser (Map Int [Char])
parseCrates column map = do
    ternChar <- parseCrate
    ignore <- option ' ' (char ' ')
    case ternChar of
        EOC -> return map
        EOL -> parseCrates 1 map
        E -> parseCrates (column + 1) map
        (C c) -> parseCrates (column + 1) (Map.insert column ((Map.findWithDefault [] column map) ++ [c]) map)

parseCrate :: Parser TernChar
parseCrate = try (EOC <$ newline <* string " 1")
         <|> EOL <$ newline
         <|> E <$ string "   "
         <|> C <$ char '[' <*> oneOf ['A'..'Z'] <* char ']'

parseColumnNumerals :: Parser String
parseColumnNumerals = some (oneOf (' ':['1'..'9']))

parseMove :: Parser (Int, Int, Int)
parseMove = (,,) <$ string "move " <*> parseNum <* string " from " <*> parseNum <* string " to " <*> parseNum <* many (newline)

parseNum :: Parser Int
parseNum = read <$> (some (oneOf ['0'..'9']))