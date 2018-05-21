module ELT0.TypeSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import ELT0.Program
import ELT0.Type

spec :: Spec
spec = do
  describe "program" $
    it "typechecks a program" $ do
      Program []                                       `program` mempty                      `shouldBe` return ()
      Program [Block "" [] Nothing]                    `program` Map.singleton "" (Code env) `shouldBe` return ()
      Program [Block "" [Reg 1 `Mov` wordO 9] Nothing] `program` Map.singleton "" (Code env) `shouldBe` return ()

      Program [Block "" [Reg 1 `Mov` registerO 2] Nothing] `program` Map.singleton "" (Code env) `shouldBe` Nothing
