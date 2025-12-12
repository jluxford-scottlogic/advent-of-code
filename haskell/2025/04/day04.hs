import Text.ParserCombinators.Parsec
import Control.Applicative (some)

type ParsedType = [[Int]]

type SplitArr a = ([a], [a])

data Flag a = NoFlag a | Flag a
    deriving Show

removeFlag :: Flag a -> a
removeFlag (NoFlag a) = a
removeFlag (Flag a) = a

setFlag :: (a -> Bool) -> Flag a -> Flag a
setFlag f (NoFlag a) | f a = Flag a
                     | otherwise = NoFlag a
setFlag _ (Flag a) = Flag a

sumIndicator :: Flag Int -> [Flag Int] -> Flag Int
sumIndicator y ns | sum (map removeFlag ns) >= 4 = y
                  | otherwise = setFlag (== 1) y

sumAround :: Show a => ((Flag a) -> [Flag a] -> (Flag a)) -> ([SplitArr (Flag a)], SplitArr (Flag a), [SplitArr (Flag a)]) -> [[Flag a]]
sumAround f ([], ([], y:b:ys), (([], z:c:zs):zss))                     = sumAround f ([],                 ([(f y [z, c, b])],                   b:ys), (([z], c:zs):zss))
sumAround f ([], (h:hs, y:b:ys), ((n:ns, z:c:zs):zss))                 = sumAround f ([],                 ((f y [h, b, n, z, c]):h:hs,          b:ys), ((z:n:ns, c:zs):zss))
sumAround f ([], (h:hs, [y]), ((n:ns, [z]):zss))                       = sumAround f ([([], reverse ((f y [h, n, z]):h:hs))], ([], reverse (z:n:ns)), zss)
sumAround f (([], x:a:xs):xss, ([], y:b:ys), (([], z:c:zs):zss))       = sumAround f (([x], a:xs):xss,    ([(f y [x, a, b, z, c])],             b:ys), (([z], c:zs):zss))
sumAround f ((t:ts, x:a:xs):xss, (h:hs, y:b:ys), ((n:ns, z:c:zs):zss)) = sumAround f ((x:t:ts, a:xs):xss, ((f y [t, x, a, h, b, n, z, c]):h:hs, b:ys), ((z:n:ns, c:zs):zss))
sumAround f ((t:ts, [x]):xss, (h:hs, [y]), ((n:ns, [z]):zss))          = sumAround f (([], reverse ((f y [t, x, h, n, z]):h:hs)):([], reverse (x:t:ts)):xss, ([], reverse (z:n:ns)), zss)
sumAround f (([], x:a:xs):xss, ([], y:b:ys), [])                       = sumAround f (([x], a:xs):xss,    ([(f y [x, a, b])],                   b:ys), [])
sumAround f ((t:ts, x:a:xs):xss, (h:hs, y:b:ys), [])                   = sumAround f ((x:t:ts, a:xs):xss, ((f y [t, x, a, h, b]):h:hs,          b:ys), [])
sumAround f ((t:ts, [x]):xss, (h:hs, [y]), [])                         = (reverse ((f y [t, x, h]):h:hs)) : (reverse (x:t:ts)) : (fmap snd xss)
sumAround f inp = error (show inp)

oneIfFlag :: Flag a -> Int
oneIfFlag (Flag _) = 1
oneIfFlag _ = 0

oneRunThrough (bs:bss) = (sumAround sumIndicator ([], ([], (fmap NoFlag bs)), fmap (([],) . (fmap NoFlag)) bss))

countRun = sum . (fmap oneIfFlag) . concat

stage1 :: [[Int]] -> Int
stage1 = countRun . oneRunThrough

zeroOnFlag :: Flag Int -> Int
zeroOnFlag (Flag _) = 0
zeroOnFlag (NoFlag n) = n

updateBasedOnFlag :: [[Flag Int]] -> [[Int]]
updateBasedOnFlag = fmap (fmap zeroOnFlag)

stage2 :: [[Int]] -> Int
stage2 bs | countCs == 0 = 0
          | otherwise = countCs + (stage2 (updateBasedOnFlag cs)) where
    cs = oneRunThrough bs
    countCs = countRun cs

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedFile :: String -> IO (Either ParseError ParsedType)
parsedFile f = parseFromFile usedParser f

parsedTest :: IO (Either ParseError ParsedType)
parsedTest = parseFromFile usedParser "test.txt"

parsedInput :: IO (Either ParseError ParsedType)
parsedInput = parseFromFile usedParser "input.txt"

usedParser :: Parser ParsedType
usedParser = some ((some parseNum) <* spaces) <* eof

parseNum :: Parser Int
parseNum = 0 <$ char '.' <|> 1 <$ char '@'