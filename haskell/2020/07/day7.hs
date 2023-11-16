import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Map

data Bag = Bag String [(Int, String)]
    deriving (Show, Eq)

convertBagsToMap :: [Bag] -> Map String [(Int, String)]
convertBagsToMap [] = empty
convertBagsToMap (Bag s ss : bs) = insert s ss (convertBagsToMap bs)

listOfAllBags :: [Bag] -> [String]
listOfAllBags = fmap (\(Bag s _) -> s)

shinyGold :: String
shinyGold = "shiny gold"

reduceBags :: Map String [(Int, String)] -> [String] -> [String]
reduceBags map [] = []
reduceBags map (s:ss) | shinyGold `elem` stepValues = s : reduceBags map ss
                      | shinyGold `elem` fmap snd newValues = s : reduceBags map' ss
                      | map ! s == [] = reduceBags map ss
                      | otherwise = reduceBags map' ss' where
    stepValues = fmap snd (map ! s)
    newValues = concatMap (map !) stepValues
    map' = insert s newValues map
    ss' = reverse (s : reverse ss)

goDepthBags :: String -> Map String [(Int, String)] -> Int
goDepthBags string map = sum step + 1 where 
    values = map ! string
    step = fmap (\(a,b) -> a * goDepthBags b map) values

findNumShinys :: [Bag] -> Int
findNumShinys bs = length $ reduceBags (convertBagsToMap bs) (listOfAllBags bs)

findNumShinysList :: [Bag] -> [String]
findNumShinysList bs = reduceBags (convertBagsToMap bs) (listOfAllBags bs)

findDepthBags :: [Bag] -> Int 
findDepthBags bs = goDepthBags shinyGold (convertBagsToMap bs) - 1

inputParser :: Parser [Bag]
inputParser = (some bagParser <* many (char '\n')) <* eof

bagParser :: Parser Bag
bagParser = Bag <$> parseBagString
                <* string " bags contain "
                <*> (some (containerParser <* many (oneOf ", ")) <|> [] <$ string "no other bags")
                <* string "."
                <* many space

containerParser :: Parser (Int, String)
containerParser = (,) <$> (read <$> some digit) <* char ' ' <*> parseBagString <* string " bag" <* many (char 's')

parseBagString :: Parser String
parseBagString = (++) <$> some (oneOf ['a'..'z']) <*> ((++) <$> string " " <*> some (oneOf ['a'..'z']))

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [Bag])
parsedValue = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day7Input.txt"

parsedTest :: IO (Either ParseError [Bag])
parsedTest = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day7TestInput.txt"

parsedTest2 :: IO (Either ParseError [Bag])
parsedTest2 = parseFromFile inputParser "C:/Dev/advent-of-code/2020/day7TestInput2.txt"