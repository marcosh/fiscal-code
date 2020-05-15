module Data.FiscalCode.Internal.GenerateFiscalCode
  ( generateSurname
  , generateName
  )
  where

-- base
import           Control.Applicative   (liftA2)
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

generateName :: Name -> String
generateName (Name name) =
  toUpper <$> take 3 (filteredNameConsonants ++ nameVowels ++ repeat 'X')
    where
      (nameConsonants, nameVowels) = partition isConsonant name

      filteredNameConsonants =
        if length nameConsonants >= 4
        then liftA2 (:) head (drop 2) nameConsonants
        else nameConsonants
