module Data.FiscalCode.Internal.GenerateFiscalCodeSpec where

-- hspec
import           Test.Hspec

import           Data.FiscalCode.Internal.GenerateFiscalCode
import           Data.FiscalCode.Types

spec :: Spec
spec =
  describe "Fiscal code" $ do

    describe "generateSurname" $ do

      it "uses the first three consonants of the surname if present" $
        generateSurname (Surname "Perone") == "PRN"

      it "uses the first three consonants from multiple surnames" $
        generateSurname (Surname "De Zan") == "DZN"

      it "uses some vowels if there are not enough consonants" $
        generateSurname (Surname "Pero") == "PRE"

      it "uses X if there are not enough consonants and vowels" $
        generateSurname (Surname "Pe") == "PEX"
