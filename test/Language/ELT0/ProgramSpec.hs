module Language.ELT0.ProgramSpec where

import Test.Hspec

import Language.ELT0.Program

spec :: Spec
spec = do
  describe "display" $
    it "displays a data" $ do
      let i = Reg 0 `Mov` STApp (wordO 123) []
      display i                           `shouldBe` "mov R0 123"
      display (Block "x" env [i] Nothing) `shouldBe` "x Code:\nmov R0 123\nhalt"
