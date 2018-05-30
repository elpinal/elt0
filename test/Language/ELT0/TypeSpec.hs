module Language.ELT0.TypeSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Language.ELT0.Program
import Language.ELT0.Type

spec :: Spec
spec = do
  describe "program" $
    it "typechecks a program" $ do
      let block l is mp = Block l env is mp

      Program []                                       `program` mempty                      `shouldBe` return ()
      Program [block "" [] Nothing]                    `program` Map.singleton "" (Code env) `shouldBe` return ()
      Program [block "" [Reg 1 `Mov` wordO 9] Nothing] `program` Map.singleton "" (Code env) `shouldBe` return ()

      Program [block "" [Reg 1 `Mov` registerO 2] Nothing] `program` Map.singleton "" (Code env) `shouldBe` Left (UnboundRegister $ Reg 2)
