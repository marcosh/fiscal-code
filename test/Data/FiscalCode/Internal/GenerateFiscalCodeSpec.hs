module Data.FiscalCode.Internal.GenerateFiscalCodeSpec where

-- hourglass
import           Time.Types                                  (Date (..),
                                                              Month (..))

-- hspec
import           Test.Hspec

import           Data.FiscalCode.Internal.GenerateFiscalCode
import           Data.FiscalCode.Types

spec :: Spec
spec =
  describe "Fiscal code" $ do

    describe "generateSurname" $ do

      it "uses the first three consonants of the surname if present" $
        generateSurname (Surname "Perone") `shouldBe` "PRN"

      it "uses the first three consonants from multiple surnames" $
        generateSurname (Surname "De Zan") `shouldBe` "DZN"

      it "uses some vowels if there are not enough consonants" $
        generateSurname (Surname "Pero") `shouldBe` "PRE"

      it "uses X if there are not enough consonants and vowels" $
        generateSurname (Surname "Pe") `shouldBe` "PEX"

    describe "generateName" $ do

      it "uses the first, third and fourth consonants if there are at least four" $
        generateName (Name "Fabrizio") `shouldBe` "FRZ"

      it "uses the consonants if there are exactly three" $
        generateName (Name "Marco") `shouldBe` "MRC"

      it "uses some vowels if there are not enough consonancts" $
        generateName (Name "Paolo") `shouldBe` "PLA"

      it "uses X if there are not enough consonants and vowels" $
        generateName (Name "Al") `shouldBe` "LAX"

    describe "generateYear" $ do

      it "uses the last two digits of 2019" $
        generateYear (Date 2019 May 15) `shouldBe` "19"

      it "uses the last two digits of 123" $
        generateYear (Date 123 May 28) `shouldBe` "23"

      it "uses the digits of 42" $
        generateYear (Date 42 May 28) `shouldBe` "42"

      it "adds a zero to 7" $
        generateYear (Date 7 May 28) `shouldBe` "07"

      it "adds a zero to 0" $
        generateYear (Date 0 May 28) `shouldBe` "00"

    describe "generateMonth" $ do

      it "returns \"A\" for January" $
        generateMonth (Date 2020 January 01) `shouldBe` "A"

      it "returns \"S\" for November" $
        generateMonth (Date 2020 November 01) `shouldBe` "S"

    describe "generateDay" $ do

      it "returns the correct date for a male" $
        generateDay (Date 1983 November 14) Male `shouldBe` "14"

      it "returns the correct date for a male on a single digit day" $
        generateDay (Date 1983 November 3) Male `shouldBe` "03"

      it "returns the correct date for a female" $
        generateDay (Date 1983 November 30) Female `shouldBe` "70"
