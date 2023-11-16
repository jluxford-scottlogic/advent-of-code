import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data CMD = CDUP | CDDOWN String | LS [Ref]
    deriving (Show, Eq)

data Ref = Dir String | Itm Integer String
    deriving (Show, Eq)

data LabelTree a = LabelTree [String] [LabelTree a] a
    deriving (Show, Eq)

instance Functor LabelTree where
    fmap f (LabelTree str childTrees val) = LabelTree str (fmap (fmap f) childTrees) (f val)

instance Foldable LabelTree where
    foldr f b (LabelTree str [] a) = f a b
    foldr f b (LabelTree str (c:cs) a) = foldr f (foldr f b c) (LabelTree str cs a)

type ParsedType = [CMD]

stage1 :: [CMD] -> Integer
stage1 = (foldr f 0) . evalTree . constructTree where
    f :: Integer -> Integer -> Integer
    f b a | b <= 100000 = b + a
          | otherwise = a

stage2 :: [CMD] -> Integer
stage2 cmds = foldr f maxVal tree where
    tree@(LabelTree s childTrees maxVal) = evalTree (constructTree cmds)
    desiredLoss = maxVal - 40000000
    f :: Integer -> Integer -> Integer
    f b a | b >= desiredLoss && b < a = b
          | otherwise = a

evalTree :: LabelTree [(String, Integer)] -> LabelTree Integer
evalTree = sumWithChildren . fmap (sum . fmap snd) where

sumWithChildren :: LabelTree Integer -> LabelTree Integer
sumWithChildren (LabelTree str childTrees val) = LabelTree str updatedChildTrees newVal where
    updatedChildTrees = fmap sumWithChildren childTrees
    newVal = val + (sum . fmap (\(LabelTree _ _ v) -> v) $ updatedChildTrees)

constructTree :: [CMD] -> LabelTree [(String, Integer)]
constructTree = constructTreeFromMaps ["/"] . (constructMapFromCmd [])

-- Constructing tree from Map is a bit of an undesired work around as worse big O but was easier
constructTreeFromMaps :: [String] -> (Map [String] [(String, Integer)], Map [String] [[String]]) -> LabelTree [(String, Integer)]
constructTreeFromMaps currKey (mapVals, mapDirs) = LabelTree currKey (fmap childTrees labelDirs) labelValues where
    labelValues = Map.findWithDefault [] currKey mapVals
    labelDirs = Map.findWithDefault [] currKey mapDirs
    childTrees a = constructTreeFromMaps a (mapVals, mapDirs)

constructMapFromCmd :: [String] -> [CMD] -> (Map [String] [(String, Integer)], Map [String] [[String]])
constructMapFromCmd _ [] = (Map.empty, Map.empty)
constructMapFromCmd s (CDDOWN key : LS refs : cmds) = (newMapValues, newMapDirs) where
    (mapValues, mapDirs) = constructMapFromCmd (key:s) cmds
    (dirs, items) = splitRefs refs
    newMapValues = Map.insert (key:s) items mapValues
    newMapDirs = Map.insert (key:s) (fmap (\dir -> dir:key:s) dirs) mapDirs
constructMapFromCmd (s:ss) (CDUP : cmds) = constructMapFromCmd ss cmds
constructMapFromCmd s (CDDOWN key : cmds) = constructMapFromCmd (key:s) cmds
constructMapFromCmd s (_:cmds) = constructMapFromCmd s cmds

splitRefs :: [Ref] -> ([String], [(String, Integer)])
splitRefs [] = ([],[])
splitRefs (Itm n s : refs) = (\(a,b) -> (a, (s,n):b)) (splitRefs refs)
splitRefs (Dir s : refs)   = (\(a,b) -> (s:a, b))     (splitRefs refs)

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (parseCommand) <* eof

parseCommand :: Parser CMD
parseCommand = LS <$ try (string "$ ls") <* newline <*> (many parseRef)
           <|> CDUP <$ try (string "$ cd ..") <* newline
           <|> CDDOWN <$ try (string "$ cd ") <*> (some $ oneOf ('/':['a'..'z'])) <* newline

parseRef :: Parser Ref
parseRef = Dir <$ string "dir " <*> (some $ oneOf ['a'..'z']) <* newline
       <|> Itm <$> parseNum <* many space <*> some (oneOf $ '.':['A'..'z']) <* newline

parseNum :: Parser Integer
parseNum = read <$> (some (oneOf ['0'..'9']))