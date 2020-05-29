module Data.FiscalCodeSpec where

-- hourglass
import           Time.Types            (Date (..), Month (..))

-- hspec
import           Test.Hspec

import           Data.FiscalCode
import           Data.FiscalCode.Types

spec :: Spec
spec =
  describe "Fiscal code" $ do
    let myFiscalCode  = generateFiscalCode (Surname "Perone") (Name "Marco")   (Date 1983 November 14) Male   (BirthPlace "C957")
        herFiscalCode = generateFiscalCode (Surname "De Zan") (Name "Cecilia") (Date 1983 November 30) Female (BirthPlace "L407")

    describe "generateFiscalCode" $ do

      it "generates correctly my fiscal code" $
        myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "83" "S" "14" "C957" "V"

      it "generates correctly her fiscal code" $
        herFiscalCode `shouldBe` FiscalCode "DZN" "CCL" "83" "S" "70" "L407" "T"

    describe "omocodia" $ do

      it "changes the last digit of my fiscal code" $
        omocodia myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "83" "S" "14" "C95T" "V"

      it "changes the last two digits of my fiscal code" $
        (omocodia . omocodia) myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "83" "S" "14" "C9RT" "V"

      it "changes the last three digits of my fiscal code" $
        (omocodia . omocodia . omocodia) myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "83" "S" "14" "CVRT" "V"

      it "changes the last four digits of my fiscal code" $
        (omocodia . omocodia . omocodia . omocodia) myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "83" "S" "1Q" "CVRT" "V"

      it "changes the last five digits of my fiscal code" $
        (omocodia . omocodia . omocodia . omocodia . omocodia) myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "83" "S" "MQ" "CVRT" "V"

      it "changes the last six digits of my fiscal code" $
        (omocodia . omocodia . omocodia . omocodia . omocodia . omocodia) myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "8P" "S" "MQ" "CVRT" "V"

      it "changes the last seven digits of my fiscal code" $
        (omocodia . omocodia . omocodia . omocodia . omocodia . omocodia . omocodia) myFiscalCode `shouldBe` FiscalCode "PRN" "MRC" "UP" "S" "MQ" "CVRT" "V"
