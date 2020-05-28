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
generateFiscalCode surname name date gender birthPlace =
  FiscalCode fcSurname fcName fcYear fcMonth fcDay fcBirthPlace fcControlCode
    where
      fcSurname = generateSurname surname

      fcName = generateName name

      fcYear = generateYear date

      fcMonth = generateMonth date

      fcDay = generateDay date gender

      fcBirthPlace = generateBirthPlace birthPlace

      fcControlCode = generateControlCode $ fcSurname ++ fcName ++ fcYear ++ fcMonth ++ fcDay ++ fcBirthPlace
