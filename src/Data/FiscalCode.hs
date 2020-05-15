module Data.FiscalCode
  ( Surname(..)
  , Name(..)
  , Gender(..)
  , BirthPlace(..)
  , FiscalCode
  , generateFiscalCode
  )
  where

-- hourglass
import           Time.Types

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
  , _fcBirthPlae   :: String -- one letter and three alphanumeric characters
  , _fcControlCode :: String -- one letter
  }

generateFiscalCode :: Surname -> Name -> Date -> Gender -> BirthPlace -> FiscalCode
generateFiscalCode (Surname surname) (Name name) (Date year month day) gender (BirthPlace birthPlace) =
  _asdf
