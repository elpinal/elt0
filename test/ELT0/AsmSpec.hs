{-# LANGUAGE BinaryLiterals #-}

module ELT0.AsmSpec where

import Test.Hspec

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as Map

import ELT0.Asm
import ELT0.Program

spec :: Spec
spec = do
  describe "block" $
    it "assembles a block" $ do
      (B.unpack . toLazyByteString) (block [] Nothing mempty)                            `shouldBe` [10]
      (B.unpack . toLazyByteString) (block [] (Just $ registerP 5) mempty)               `shouldBe` [0b00001001, 5]
      (B.unpack . toLazyByteString) (block [] (Just $ labelP "a") $ Map.singleton "a" 9) `shouldBe` [0b00101001, 0, 0, 0, 9]

      (B.unpack . toLazyByteString) (block [Mov (Reg 2) (wordO 5)] Nothing mempty)                       `shouldBe` [0b00100000, 2, 0, 0, 0, 5, 10]
      (B.unpack . toLazyByteString) (block [Mov (Reg 0) (registerO 9)] Nothing mempty)                   `shouldBe` [0, 0, 9, 10]
      (B.unpack . toLazyByteString) (block [Mov (Reg 64) (labelO "x")] Nothing $ Map.singleton "x" 1000) `shouldBe` [0b00100000, 64, 0, 0, 0b11, 0b11101000, 10]
      (B.unpack . toLazyByteString) (block [Shr (Reg 128) (wordN 3) (registerN 200)] Nothing mempty)     `shouldBe` [0b00100111, 128, 0, 0, 0, 3, 200, 10]
