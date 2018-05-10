{-# LANGUAGE BinaryLiterals #-}

module ELT0.EvalSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import ELT0.Eval

spec :: Spec
spec = do
  describe "run & runFile" $
    it "evaluates a code, then returns the result register file" $ do
      -- "halt"
      run (code [10]) `shouldBe` Map.empty

      -- "mov"
      run (code [0b00100000, 45, 0, 0, 0, 1, 10])            `shouldBe` Map.singleton 45 1
      run (code [0b00100000, 45, 0, 0, 0, 1, 0, 76, 45, 10]) `shouldBe` Map.fromList [(45, 1), (76, 1)]

      -- "add"
      run (code [0b01100001, 13, 0, 0, 0, 1, 0, 0, 0, 3, 10])                     `shouldBe` Map.singleton 13 4
      run (code [0b01100001, 13, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 3, 10])         `shouldBe` Map.singleton 13 2
      run (code [0b00100000, 13, 0, 0, 0, 9, 0b01000001, 88, 13, 0, 0, 0, 7, 10]) `shouldBe` Map.fromList [(13, 9), (88, 16)]

      -- "sub"
      run (code [0b01100010, 13, 0, 0, 0, 3, 0, 0, 0, 1, 10]) `shouldBe` Map.singleton 13 2
      run (code [0b01100010, 13, 1, 1, 1, 1, 1, 1, 1, 2, 10]) `shouldBe` Map.singleton 13 0xffffffff

      -- "and"
      run (code [0b01100011, 7, 0, 0, 0, 11, 0, 0, 0, 13, 10]) `shouldBe` Map.singleton 7 9
      run (code [0b01100011, 7, 1, 1, 1, 1, 0, 0, 0, 1, 10])   `shouldBe` Map.singleton 7 1

      -- "or"
      run (code [0b01100100, 7, 0, 0, 0, 11, 0, 0, 0, 13, 10]) `shouldBe` Map.singleton 7 15
      run (code [0b01100100, 7, 1, 1, 1, 1, 0, 0, 0, 1, 10])   `shouldBe` Map.singleton 7 16843009

      -- "not"
      run (code [0b01100101, 7, 0xff, 0xff, 0xff, 11, 10]) `shouldBe` Map.singleton 7 244
      run (code [0b01100101, 7, 0xff, 0xff, 0, 3, 10])     `shouldBe` Map.singleton 7 65532

      -- "shl"
      run (code [0b01100110, 7, 0, 0, 0, 11, 0, 0, 0, 1, 10])            `shouldBe` Map.singleton 7 22
      run (code [0b01100110, 7, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 1, 10]) `shouldBe` Map.singleton 7 4294967294

      -- "shr"
      run (code [0b01100111, 7, 0, 0, 0, 11, 0, 0, 0, 1, 10]) `shouldBe` Map.singleton 7 5
      run (code [0b01100111, 7, 0, 0, 1, 1, 0, 0, 0, 1, 10])  `shouldBe` Map.singleton 7 128
      run (code [0b01100111, 7, 0, 0, 2, 1, 0, 0, 0, 1, 10])  `shouldBe` Map.singleton 7 256
      run (code [0b01100111, 7, 0, 0, 8, 1, 0, 0, 0, 1, 10])  `shouldBe` Map.singleton 7 1024

      -- "if jmp"
      run (code [0b00100000, 66, 0, 0, 0, 9, 8, 66, 66, 10])                              `shouldBe` Map.singleton 66 9
      run (code [0b00100000, 66, 0, 0, 0, 13, 8, 66, 66, 0, 33, 66, 10])                  `shouldBe` Map.fromList [(66, 13), (33, 13)]
      run (code [0b00100000, 66, 0, 0, 0, 0, 0b00101000, 66, 0, 0, 0, 16, 0, 33, 66, 10]) `shouldBe` Map.singleton 66 0
      let m = (Map.fromList [(121, 1), (7, 7)])
      runFile (code [8, 121, 7, 0, 8, 121, 10]) m `shouldBe` Map.insert 8 1 m
      let m = (Map.fromList [(121, 0), (7, 7)])
      runFile (code [8, 121, 7, 0, 8, 121, 10]) m `shouldBe` m

      -- "jmp"
      run (code [0b00100000, 66, 0, 0, 0, 9, 9, 66, 10])                               `shouldBe` Map.singleton 66 9
      run (code [0b00100000, 66, 0, 0, 0, 12, 9, 66, 0, 33, 66, 10])                   `shouldBe` Map.singleton 66 12
      run (code [0b00100000, 66, 0, 0, 0, 9, 9, 66, 0, 33, 66, 10])                    `shouldBe` Map.fromList [(66, 9), (33, 9)]
      run (code [0b00101001, 0, 0, 0, 6, 10])                                          `shouldBe` Map.empty
      run (code [0b00101001, 0, 0, 0, 8, 0xff, 0xff, 0b00100000, 8, 0, 0, 0, 230, 10]) `shouldBe` Map.singleton 8 230

      -- "salloc"
      run (code [11, 0, 0, 0, 1, 10])                         `shouldBe` Map.empty
      runStack (code [11, 0, 0, 0, 1, 10]) []                 `shouldBe` [0]
      runStack (code [11, 0, 0, 0, 4, 10]) []                 `shouldBe` [0, 0, 0, 0]
      runStack (code [11, 0, 0, 0, 2, 11, 0, 0, 0, 5, 10]) [] `shouldBe` replicate 7 0
