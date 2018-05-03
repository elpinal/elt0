module ELT0.EvalSpec where

import Test.Hspec

import ELT0.Eval

spec :: Spec
spec = do
  describe "run" $
    it "evaluates a code, then returns the result register file" $ do
      run (code [10]) `shouldBe` Nothing
