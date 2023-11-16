
import Text.ParserCombinators.Parsec
import Control.Applicative (some)
import Data.Functor
import Data.Char (digitToInt)
import Distribution.Simple.Command (OptDescr(BoolOpt))

data PassportInfo = Byr String PassportInfo
                  | Iyr String PassportInfo
                  | Eyr String PassportInfo
                  | Hgt String PassportInfo
                  | Hcl String PassportInfo
                  | Ecl String PassportInfo
                  | Pid String PassportInfo
                  | Cid String PassportInfo
                  | Empty
        deriving(Show, Eq)

bitifyPassport :: PassportInfo -> Int
bitifyPassport (Byr _ p) = 2 + bitifyPassport p
bitifyPassport (Iyr _ p) = 4 + bitifyPassport p
bitifyPassport (Eyr _ p) = 8 + bitifyPassport p
bitifyPassport (Hgt _ p) = 16 + bitifyPassport p
bitifyPassport (Hcl _ p) = 32 + bitifyPassport p
bitifyPassport (Ecl _ p) = 64 + bitifyPassport p
bitifyPassport (Pid _ p) = 128 + bitifyPassport p
bitifyPassport (Cid _ p) = 1 + bitifyPassport p
bitifyPassport Empty = 0

validPassport :: PassportInfo -> Bool
validPassport = (>= 254) . bitifyPassport

validValues :: PassportInfo -> Bool
validValues (Byr s p) = elem s valids && validValues p where
    valids = [['1', '9', y, z] | y <- ['2'..'9'], z <- ['0'..'9']] ++ ["2000", "2001", "2002"]
validValues (Iyr s p) = elem s valids && validValues p where
    valids = "2020":[['2', '0', '1', x] | x <- ['0'..'9']]
validValues (Eyr s p) = elem s valids && validValues p where
    valids = "2030":[['2', '0', '2', x] | x <- ['0'..'9']]
validValues (Hgt s p) = elem s valids && validValues p where
    valids = ["190cm", "191cm", "192cm", "193cm"] ++ [['1', z, x] ++ "cm" | z <- ['5'..'8'], x <- ['0'..'9']]
           ++ "59in":[['6', x] ++ "in" | x <- ['0'..'9']] ++ [['7', x] ++ "in" | x <- ['0'..'6']]
validValues (Hcl (s:ss) p) | s == '#' = length ss == 6 && f ss && validValues p
                           | otherwise = False where
    f :: String -> Bool
    f []     = True 
    f (s:ss) = elem s (['0'..'9'] ++ ['a'..'f']) && f ss
validValues (Ecl s p) = elem s valids && validValues p where
    valids = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validValues (Pid s p) = length s == 9 && f s && validValues p where
    f :: String -> Bool
    f []     = True 
    f (s:ss) = elem s ['0'..'9'] && f ss
validValues (Cid s p) = validValues p
validValues Empty = True

validPassports :: [PassportInfo] -> Int
validPassports = length . filter validPassport

validPassportsAndValues :: [PassportInfo] -> Int
validPassportsAndValues = length . filter validValues . filter validPassport

day4Parser :: Parser [PassportInfo]
day4Parser = some passportInfoParser <* (eof <|> (char '\n' $> () ))

passportInfoParser :: Parser PassportInfo
passportInfoParser =  try (Byr <$ string "byr:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> try (Iyr <$ string "iyr:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> try (Eyr <$ string "eyr:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> try (Hgt <$ string "hgt:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> try (Hcl <$ string "hcl:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> try (Ecl <$ string "ecl:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> try (Pid <$ string "pid:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> try (Cid <$ string "cid:" <*> some (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "#")) <* space <*> passportInfoParser)
                  <|> Empty <$ char '\n'

runOnInput :: (a -> b) ->  IO (Either ParseError a) -> IO (Either ParseError b)
runOnInput = fmap . fmap

parsedValue :: IO (Either ParseError [PassportInfo])
parsedValue = parseFromFile day4Parser "C:/Dev/advent-of-code/2020/day4Input.txt"