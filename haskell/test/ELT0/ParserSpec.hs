module ELT0.ParserSpec where

import Test.Hspec

import Data.Either

import ELT0.Parser
import ELT0.Program

spec :: Spec
spec = do
  describe "mainParser" $ do
    it "parses a program" $ do
      let prog = return . Program

      mainParser ""  `shouldBe` prog []
      mainParser " " `shouldBe` prog []

      mainParser "main:\n jmp main"   `shouldBe` prog [Block "main" [] $ labelO "main"]
      mainParser "main:\n jmp main\n" `shouldBe` prog [Block "main" [] $ labelO "main"]
      -- mainParser "main: jmp main" `shouldSatisfy` isLeft
      mainParser "main:\n mov R0 1\n jmp next"             `shouldBe` prog [Block "main" [Reg 0 `Mov` wordO 1] $ labelO "next"]
      mainParser "x:\nshr R255 1000 288\nnot R0 0\n jmp a" `shouldBe` prog [Block "x" [Shr (Reg 255) (wordN 1000) (wordN 288), Reg 0 `Not` wordN 0] $ labelO "a"]

      mainParser "L1:\n jmp main"  `shouldSatisfy` isLeft -- labels must not start in upper case. 
      mainParser "mov:\n jmp main" `shouldSatisfy` isLeft -- labels is distinguished from mnemonics.
      mainParser "jmp:\n jmp main" `shouldSatisfy` isLeft

  describe "label" $ do
    it "parses a label" $ do
      runParser label [(Ident "main", newPosition 1 1), (Colon, newPosition 1 1)] `shouldBe` return (Just ("main", []))

  describe "jmp" $ do
    it "parses a jump instruction" $ do
      runParser jmp [(Jmp, newPosition 1 1), (Ident "L1", newPosition 1 1)] `shouldBe` return (Just (labelO "L1", []))

  describe "inst" $ do
    it "parses a instruction" $ do
      runParser inst [] `shouldBe` return Nothing
      runParser inst [(Mnem TMov, newPosition 1 1), (RegToken 0, newPosition 1 1), (Digits 1, newPosition 1 1)] `shouldBe` return (Just (Reg 0 `Mov` wordO 1, []))

  describe "reg" $
    it "parses a register" $ do
      runParser reg [(RegToken 0, newPosition 1 1)]             `shouldBe` return (Just (Reg 0, []))
      runParser reg [(RegToken 0, newPosition 1 1), (RegToken 1, newPosition 1 7)] `shouldBe` return (Just (Reg 0, [(RegToken 1, newPosition 1 7)]))

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
