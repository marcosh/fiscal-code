module Data.FiscalCode.Internal.OmocodiaSpec where

-- hspec
import           Test.Hspec

import           Data.FiscalCode.Internal.Omocodia

spec :: Spec
spec =
  describe "Omocodia" $

    describe "omocodia" $ do

      it "converts the last digit of ABC123DEF" $
        omocodia "ABC123DEF" `shouldBe` "ABC12PDEF"

      it "converts the last digit of 123ABC456" $
        omocodia "123ABC456" `shouldBe` "123ABC45S"
