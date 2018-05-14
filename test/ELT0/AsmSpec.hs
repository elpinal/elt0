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
