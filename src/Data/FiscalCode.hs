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

newtype FiscalCode = FiscalCode String

generateFiscalCode :: Surname -> Name -> Date -> Gender -> BirthPlace -> FiscalCode
generateFiscalCode (Surname surname) (Name name) (Date year month day) gender (BirthPlace birthPlace) =
  _asdf
