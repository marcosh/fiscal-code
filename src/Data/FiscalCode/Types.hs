module Data.FiscalCode.Types where

newtype Surname = Surname String

newtype Name = Name String

data Gender
  = Male
  | Female

-- | Belfiore code of the birth town or country
-- composed by an uppercase letter and three digits
-- see https://it.wikipedia.org/wiki/Codice_catastale
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
  deriving (Eq, Show)
