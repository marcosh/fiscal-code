module Data.FiscalCode.Internal.GenerateFiscalCode
  ( generateSurname
  , generateName
  , generateYear
  , generateMonth
  , generateDay
  , generateBirthPlace
  , generateControlCode
  )
  where

-- base
import           Control.Applicative   (liftA2)
import           Data.Char             (toLower, toUpper)
import           Data.List             (partition)

-- hourglass
import           Data.Hourglass        (TimeFormatElem (..), timePrint)
import           Time.Types            (Date (..), Month (..))

import           Data.FiscalCode.Types

isConsonant :: Char -> Bool
isConsonant = (`elem` "bcdfghjklmnpqrstvwxyz") . toLower

generateSurname :: Surname -> String
generateSurname (Surname surname) =
  toUpper <$> take 3 (surnameConsonants ++ surnameVowels ++ repeat 'X')
    where
      (surnameConsonants, surnameVowels) = partition isConsonant surname

generateName :: Name -> String
generateName (Name name) =
  toUpper <$> take 3 (filteredNameConsonants ++ nameVowels ++ repeat 'X')
    where
      (nameConsonants, nameVowels) = partition isConsonant name

      filteredNameConsonants =
        if length nameConsonants >= 4
        then liftA2 (:) head (drop 2) nameConsonants
        else nameConsonants

takeEnd :: Int -> [a] -> [a]
takeEnd i xs = f xs (drop i xs)
    where f (_:xs') (_:ys) = f xs' ys
          f xs' _          = xs'

padded :: Int -> a -> [a] -> [a]
padded n p xs = replicate diff p ++ xs
  where
    len_xs = length xs
    diff   = n - len_xs

generateYear :: Date -> String
generateYear date = padded 2 '0' $ takeEnd 2 $ timePrint [Format_Year] date

generateMonth :: Date -> String
generateMonth date = case dateMonth date of
  January   -> "A"
  February  -> "B"
  March     -> "c"
  April     -> "D"
  May       -> "E"
  June      -> "H"
  July      -> "L"
  August    -> "M"
  September -> "P"
  October   -> "R"
  November  -> "S"
  December  -> "T"

generateDay :: Date -> Gender -> String
generateDay date gender = padded 2 '0' $ show day
  where
    day = case gender of
      Male   -> dateDay date
      Female -> dateDay date + 40

generateBirthPlace :: BirthPlace -> String
generateBirthPlace (BirthPlace birthPlace) = birthPlace

uninterleave :: [a] -> ([a], [a])
uninterleave []         = ([], [])
uninterleave [x]        = ([x], [])
uninterleave (x1:x2:xs) = ([x1], [x2]) <> uninterleave xs

generateControlCode :: String -> String
generateControlCode s = convertFinal $ mod (sum ((convertOdd <$> odds) ++ (convertEven <$> evens))) 26
  where
    (odds, evens) = uninterleave s

    convertOdd :: Char -> Int
    convertOdd '0' = 1
    convertOdd '1' = 0
    convertOdd '2' = 5
    convertOdd '3' = 7
    convertOdd '4' = 9
    convertOdd '5' = 13
    convertOdd '6' = 15
    convertOdd '7' = 17
    convertOdd '8' = 19
    convertOdd '9' = 21
    convertOdd 'A' = 1
    convertOdd 'B' = 0
    convertOdd 'C' = 5
    convertOdd 'D' = 7
    convertOdd 'E' = 9
    convertOdd 'F' = 13
    convertOdd 'G' = 15
    convertOdd 'H' = 17
    convertOdd 'I' = 19
    convertOdd 'J' = 21
    convertOdd 'K' = 2
    convertOdd 'L' = 4
    convertOdd 'M' = 18
    convertOdd 'N' = 20
    convertOdd 'O' = 11
    convertOdd 'P' = 3
    convertOdd 'Q' = 6
    convertOdd 'R' = 8
    convertOdd 'S' = 12
    convertOdd 'T' = 14
    convertOdd 'U' = 16
    convertOdd 'V' = 10
    convertOdd 'W' = 22
    convertOdd 'X' = 25
    convertOdd 'Y' = 24
    convertOdd 'Z' = 23
    convertOdd _   = 0

    convertEven :: Char -> Int
    convertEven '0' = 0
    convertEven '1' = 1
    convertEven '2' = 2
    convertEven '3' = 3
    convertEven '4' = 4
    convertEven '5' = 5
    convertEven '6' = 6
    convertEven '7' = 7
    convertEven '8' = 8
    convertEven '9' = 9
    convertEven 'A' = 0
    convertEven 'B' = 1
    convertEven 'C' = 2
    convertEven 'D' = 3
    convertEven 'E' = 4
    convertEven 'F' = 5
    convertEven 'G' = 6
    convertEven 'H' = 7
    convertEven 'I' = 8
    convertEven 'J' = 9
    convertEven 'K' = 10
    convertEven 'L' = 11
    convertEven 'M' = 12
    convertEven 'N' = 13
    convertEven 'O' = 14
    convertEven 'P' = 15
    convertEven 'Q' = 16
    convertEven 'R' = 17
    convertEven 'S' = 18
    convertEven 'T' = 19
    convertEven 'U' = 20
    convertEven 'V' = 21
    convertEven 'W' = 22
    convertEven 'X' = 23
    convertEven 'Y' = 24
    convertEven 'Z' = 25
    convertEven _   = 0

    convertFinal :: Int -> String
    convertFinal 0  = "A"
    convertFinal 1  = "B"
    convertFinal 2  = "C"
    convertFinal 3  = "D"
    convertFinal 4  = "E"
    convertFinal 5  = "F"
    convertFinal 6  = "G"
    convertFinal 7  = "H"
    convertFinal 8  = "I"
    convertFinal 9  = "J"
    convertFinal 10 = "K"
    convertFinal 11 = "L"
    convertFinal 12 = "M"
    convertFinal 13 = "N"
    convertFinal 14 = "O"
    convertFinal 15 = "P"
    convertFinal 16 = "Q"
    convertFinal 17 = "R"
    convertFinal 18 = "S"
    convertFinal 19 = "T"
    convertFinal 20 = "U"
    convertFinal 21 = "V"
    convertFinal 22 = "W"
    convertFinal 23 = "X"
    convertFinal 24 = "Y"
    convertFinal 25 = "Z"
    convertFinal _  = ""
