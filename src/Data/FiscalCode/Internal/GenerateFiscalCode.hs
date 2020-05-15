module Data.FiscalCode.Internal.GenerateFiscalCode
  ( generateSurname
  )
  where

-- base
import           Data.Char             (toLower, toUpper)
import           Data.List             (partition, take)

import           Data.FiscalCode.Types

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

isConsonant :: Char -> Bool
isConsonant = (`elem` "bcdfghjklmnpqrstvwxyz") . toLower

generateSurname :: Surname -> String
generateSurname (Surname surname) =
  toUpper <$> take 3 (surnameConsonants ++ surnameVowels ++ repeat 'X')
    where
      (surnameConsonants, surnameVowels) = partition isConsonant surname
