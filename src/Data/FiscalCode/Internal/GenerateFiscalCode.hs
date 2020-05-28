module Data.FiscalCode.Internal.GenerateFiscalCode
  ( generateSurname
  , generateName
  , generateYear
  , generateMonth
  , generateDay
  , generateBirthPlace
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
