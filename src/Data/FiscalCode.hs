module Data.FiscalCode
  ( Surname(..)
  , Name(..)
  , Gender(..)
  , BirthPlace(..)
  , FiscalCode
  , generateFiscalCode
  )
  where

-- base
import           Data.Char  (toLower)
import           Data.List  (partition, take)

-- hourglass
import           Time.Types (Date (..))

newtype Surname = Surname String

newtype Name = Name String

data Gender
  = Male
  | Female

newtype BirthPlace = BirthPlace String

data FiscalCode = FiscalCode
  { _fcSurname     :: String -- three letters
  , _fcName        :: String -- three letters
  , _fcYear        :: String -- two alphanumeric characters
  , _fcMonth       :: String -- one letter
  , _fcDay         :: String -- two alphanumeric characters
  , _fcBirthPlace  :: String -- one letter and three alphanumeric characters
  , _fcControlCode :: String -- one letter
  }

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

isConsonant :: Char -> Bool
isConsonant = not . isVowel

generateFCSurname :: Surname -> String
generateFCSurname (Surname surname) =
  take 3 $ surnameConsonants ++ surnameVowels ++ repeat 'X'
    where
      (surnameConsonants, surnameVowels) = partition isConsonant surname

generateFiscalCode :: Surname -> Name -> Date -> Gender -> BirthPlace -> FiscalCode
generateFiscalCode surname (Name name) (Date year month day) gender (BirthPlace birthPlace) =
  FiscalCode fcSurname fcName fcYear fcMonth fcDay fcBirthPlace fcControlCode
    where
      fcSurname = generateFCSurname surname

      fcName = ""

      fcYear = ""

      fcMonth = ""

      fcDay = ""

      fcBirthPlace = ""

      fcControlCode = ""
