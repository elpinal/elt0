{-# LANGUAGE BinaryLiterals #-}

module ELT0.EvalSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import ELT0.Eval

spec :: Spec
spec = do
  describe "run" $
    it "evaluates a code, then returns the result register file" $ do
      run (code [10])                                                             `shouldBe` Map.empty
      run (code [0b00100000, 45, 0, 0, 0, 1, 10])                                 `shouldBe` Map.singleton 45 1
      run (code [0b00100000, 45, 0, 0, 0, 1, 0, 76, 45, 10])                      `shouldBe` Map.fromList [(45, 1), (76, 1)]
      run (code [0b01100001, 13, 0, 0, 0, 1, 0, 0, 0, 3, 10])                     `shouldBe` Map.singleton 13 4
      run (code [0b01100001, 13, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 3, 10])         `shouldBe` Map.singleton 13 2
      run (code [0b00100000, 13, 0, 0, 0, 9, 0b01000001, 88, 13, 0, 0, 0, 7, 10]) `shouldBe` Map.fromList [(13, 9), (88, 16)]

      run (code [0b01100010, 13, 0, 0, 0, 3, 0, 0, 0, 1, 10]) `shouldBe` Map.singleton 13 2
      run (code [0b01100010, 13, 1, 1, 1, 1, 1, 1, 1, 2, 10]) `shouldBe` Map.singleton 13 0xffffffff
