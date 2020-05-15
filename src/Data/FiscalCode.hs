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
import           Time.Types                                  (Date (..))

import           Data.FiscalCode.Internal.GenerateFiscalCode
import           Data.FiscalCode.Types

generateFiscalCode :: Surname -> Name -> Date -> Gender -> BirthPlace -> FiscalCode
generateFiscalCode surname (Name name) (Date year month day) gender (BirthPlace birthPlace) =
  FiscalCode fcSurname fcName fcYear fcMonth fcDay fcBirthPlace fcControlCode
    where
      fcSurname = generateSurname surname

      fcName = ""

      fcYear = ""

      fcMonth = ""

      fcDay = ""

      fcBirthPlace = ""

      fcControlCode = ""
