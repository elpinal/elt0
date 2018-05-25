module ELT0.Parser.LexerSpec where

import Test.Hspec

import Data.Either

import ELT0.Parser.Lexer

spec :: Spec
spec = do
  describe "lex1" $
    it "lex a token" $ do
      runLexer lex1 ""          `shouldBe` return (Nothing)
      runLexer lex1 "R0"        `shouldBe` return (Just (RegToken 0, newPosition 1 1))
      runLexer lex1 " R1"       `shouldBe` return (Just (RegToken 1, newPosition 1 2))
      runLexer lex1 "R0 R1"     `shouldBe` return (Just (RegToken 0, newPosition 1 1))
      runLexer lex1 "mov R0 R1" `shouldBe` return (Just (Mnem TMov, newPosition 1 1))

      runLexer lex1 "R255"  `shouldBe` return (Just (RegToken 255, newPosition 1 1))
      runLexer lex1 "R256"  `shouldSatisfy` isLeft
      runLexer lex1 "R2560" `shouldSatisfy` isLeft

      runLexer lex1 "R0a" `shouldSatisfy` isLeft
      runLexer lex1 "0a"  `shouldSatisfy` isLeft
      runLexer lex1 "R"   `shouldSatisfy` isLeft
      runLexer lex1 "R@"  `shouldSatisfy` isLeft

  describe "lexer" $
    it "lex tokens" $ do
      runLexer lexer ""          `shouldBe` return []
      runLexer lexer "R0"        `shouldBe` return [(RegToken 0, newPosition 1 1)]
      runLexer lexer " R1"       `shouldBe` return [(RegToken 1, newPosition 1 2)]
      runLexer lexer "R0 R1"     `shouldBe` return [(RegToken 0, newPosition 1 1), (RegToken 1, newPosition 1 4)]
      runLexer lexer "mov R0 R1" `shouldBe` return [(Mnem TMov, newPosition 1 1), (RegToken 0, newPosition 1 5), (RegToken 1, newPosition 1 8)]
