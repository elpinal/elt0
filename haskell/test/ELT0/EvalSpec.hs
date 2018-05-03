{-# LANGUAGE BinaryLiterals #-}

module ELT0.EvalSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import ELT0.Eval

spec :: Spec
spec = do
  describe "run" $
    it "evaluates a code, then returns the result register file" $ do
      run (code [10]) `shouldBe` Map.empty
      run (code [0b00100000, 45, 0, 0, 0, 1, 10]) `shouldBe` Map.singleton 45 1
      run (code [0b00100000, 45, 0, 0, 0, 1, 0, 76, 45, 10]) `shouldBe` Map.fromList [(45, 1), (76, 1)]
