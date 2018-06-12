{-# LANGUAGE BinaryLiterals #-}

module Language.ELT0.AsmSpec where

import Test.Hspec

import Language.ELT0.Asm
import Language.ELT0.Program

spec :: Spec
spec = do
  describe "assemble" $
    it "assembles a program" $ do
      let f = resolve . assemble' . Program
      let block l is mp = Block l env is $ flip STApp [] <$> mp
      f [block "" [] Nothing]                                        `shouldBe` [10]
      f [block "" [] (Just $ registerP 5)]                           `shouldBe` [0b00001001, 5]
      f [block "" [] (Just $ labelP "")]                             `shouldBe` [0b00101001, 0, 0, 0, 1]
      f [block "x" [] (Just $ labelP "x")]                           `shouldBe` [0b00101001, 0, 0, 0, 1]
      f [block "" [] (Just $ labelP "x"), block "x" [] Nothing]      `shouldBe` [0b00101001, 0, 0, 0, 6, 10]
      f [block "" [Reg 3 `Mov` STApp (registerO 4) []] Nothing]      `shouldBe` [0b00000000, 3, 4, 10]
      f [block "" [Shr (Reg 0) (registerN 40) $ wordN 1024] Nothing] `shouldBe` [0b01000111, 0, 40, 0, 0, 0b0100, 0, 10]
