import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.List (transpose)
import MoveArr

type ParsedType = [[Char]]

checkXmas :: [Char] -> Int
checkXmas ('X':'M':'A':'S':xs) = 1 + checkXmas ('S':xs)
checkXmas ('S':'A':'M':'X':xs) = 1 + checkXmas ('X':xs)
checkXmas (x:xs) = checkXmas xs
checkXmas _ = 0

allFourDir :: [[Char]] -> [[[Char]]]
allFourDir css = css : transpose css : []

checkXmasOnMany :: [[[Char]]] -> Int
checkXmasOnMany = sum . fmap (sum . fmap checkXmas)

allTopCorner :: [[Char]] -> [[[Char]]]
allTopCorner css = css : reverse css : []

alignDiag :: [[Char]] -> [[Char]]
alignDiag [] = []
alignDiag ([c]:css) = [[c]]
alignDiag (cs:css) = cs : (alignDiag (fmap tail css))

allFourDiag :: [[Char]] -> [[[Char]]]
allFourDiag css = downDiag ++ upDiag where
    downDiag = (fmap (transpose . alignDiag) . allTopCorner) css
    upDiag = (fmap (transpose . alignDiag . (fmap (tail . reverse))) . allTopCorner) css

stage1 :: [[Char]] -> Int
stage1 css = linear + diagonal where
    linear = (checkXmasOnMany . allFourDir) css
    diagonal = (checkXmasOnMany . allFourDiag) css

checkForAOfMas :: MoveArr Char -> Int
checkForAOfMas arr | atEnd arr = 0
                   | peekXY (0,0) arr == Just 'A' = checkForLeadingMSOfMas arr + (checkForAOfMas nextArr)
                   | otherwise = checkForAOfMas nextArr where
                    Just nextArr = stepThrough arr

checkForLeadingMSOfMas :: MoveArr Char -> Int
checkForLeadingMSOfMas arr | (peekXY (1,1) arr == Just 'M') && (peekXY (-1, -1) arr == Just 'S') = checkForFallingMSOfMas arr
                           | (peekXY (1,1) arr == Just 'S') && (peekXY (-1, -1) arr == Just 'M') = checkForFallingMSOfMas arr
                           | otherwise = 0

checkForFallingMSOfMas :: MoveArr Char -> Int
checkForFallingMSOfMas arr | (peekXY (1,-1) arr == Just 'M') && (peekXY (-1, 1) arr == Just 'S') = 1
                           | (peekXY (1,-1) arr == Just 'S') && (peekXY (-1, 1) arr == Just 'M') = 1
                           | otherwise = 0

stage2 :: [[Char]] -> Int
stage2 = checkForAOfMas . fromArr

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some (some parseChar <* spaces) <* eof

parseChar :: Parser Char
parseChar = oneOf "XMAS"