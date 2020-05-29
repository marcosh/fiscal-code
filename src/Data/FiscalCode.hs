module Data.FiscalCode
  ( Surname(..)
  , Name(..)
  , Gender(..)
  , BirthPlace(..)
  , FiscalCode
  , generateFiscalCode
  , omocodia
  )
  where

-- hourglass
import           Time.Types                                  (Date (..))

import           Data.FiscalCode.Internal.GenerateFiscalCode
import qualified Data.FiscalCode.Internal.Omocodia           as FCO
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

readFC :: String -> FiscalCode
readFC s = FiscalCode surname name year month day birthPlace controlCode
  where
    (surname, t)              = splitAt 3 s
    (name, u)                 = splitAt 3 t
    (year, v)                 = splitAt 2 u
    (month, w)                = splitAt 1 v
    (day, x)                  = splitAt 1 w
    (birthPlace, controlCode) = splitAt 4 x

omocodia :: FiscalCode -> FiscalCode
omocodia = readFC . FCO.omocodia . show
