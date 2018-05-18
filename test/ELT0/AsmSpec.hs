{-# LANGUAGE BinaryLiterals #-}

module ELT0.AsmSpec where

import Test.Hspec

import ELT0.Asm
import ELT0.Program

spec :: Spec
spec = do
  describe "assemble" $
    it "assembles a program" $ do
      let f = resolve . assemble' . Program
      f [Block "" [] Nothing]                                        `shouldBe` [10]
      f [Block "" [] (Just $ registerP 5)]                           `shouldBe` [0b00001001, 5]
      f [Block "" [] (Just $ labelP "")]                             `shouldBe` [0b00101001, 0, 0, 0, 1]
      f [Block "x" [] (Just $ labelP "x")]                           `shouldBe` [0b00101001, 0, 0, 0, 1]
      f [Block "" [] (Just $ labelP "x"), Block "x" [] Nothing]      `shouldBe` [0b00101001, 0, 0, 0, 6, 10]
      f [Block "" [Reg 3 `Mov` registerO 4] Nothing]                 `shouldBe` [0b00000000, 3, 4, 10]
      f [Block "" [Shr (Reg 0) (registerN 40) $ wordN 1024] Nothing] `shouldBe` [0b01000111, 0, 40, 0, 0, 0b0100, 0, 10]
