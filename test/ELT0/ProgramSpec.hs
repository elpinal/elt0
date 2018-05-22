module ELT0.ProgramSpec where

import Test.Hspec

import ELT0.Program

spec :: Spec
spec = do
  describe "display" $
    it "displays a data" $ do
      let i = Reg 0 `Mov` wordO 123
      display i                       `shouldBe` "mov R0 123"
      display (Block "x" [i] Nothing) `shouldBe` "x:\nmov R0 123\nhalt"
