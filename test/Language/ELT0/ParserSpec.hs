module Language.ELT0.ParserSpec where

import Test.Hspec

import Language.ELT0.Parser

spec :: Spec
spec = do
  describe "minimal" $
    it "parses a token" $ do
      runMinimal comma Comma             `shouldBe` Parsed1 ()
      runMinimal comma LBrace            `shouldBe` Other1
      runMinimal (comma -|- comma) Comma `shouldBe` Parsed1 ()

  describe "parser" $
    it "parses tokens" $ do
      let p = fromMinimal comma
      let o = option comma
      let t = (Colon, newPosition 1 1)

      runParser p [(Comma, newPosition 1 1)] `shouldBe` (Parsed (), [])
      runParser p [t]                        `shouldBe` (Fail (Just t) ["comma"], [])

      runParser o [(Comma, newPosition 1 1)] `shouldBe` (Parsed $ Just (), [])
      runParser o [t]                        `shouldBe` (Parsed Nothing, [t])
