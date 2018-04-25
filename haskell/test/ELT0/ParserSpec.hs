module ELT0.ParserSpec where

import Test.Hspec

import Data.Either

import ELT0.Parser
import ELT0.Program

spec :: Spec
spec = do
  let word = Value . Word

  describe "mainParser" $ do
    it "parses a program" $ do
      let prog = return . Program

      mainParser ""  `shouldBe` prog []
      mainParser " " `shouldBe` prog []

      mainParser "main:\n jmp main" `shouldBe` prog [Block "main" [] "main"]
      -- mainParser "main: jmp main" `shouldSatisfy` isLeft
      mainParser "main:\n mov R0 1\n jmp next" `shouldBe` prog [Block "main" [Reg 0 `Mov` word 1] "next"]
      mainParser "x:\nshr R255 1000 288\nnot R0 0\n jmp a" `shouldBe` prog [Block "x" [Shr (Reg 255) (word 1000) (word 288), Reg 0 `Not` word 0] "a"]

      mainParser "L1:\n jmp main" `shouldSatisfy` isLeft -- labels must not start in upper case. 
      -- mainParser "mov:\n jmp main" `shouldSatisfy` isLeft -- labels is distinguished from mnemonics.

  describe "label" $ do
    it "parses a label" $ do
      runParser label [Ident "main", Colon] `shouldBe` return (Just ("main", []))

  describe "jmp" $ do
    it "parses a jump instruction" $ do
      runParser jmp [Ident "jmp", Ident "L1"] `shouldBe` return (Just ("L1", []))

  describe "inst" $ do
    it "parses a instruction" $ do
      runParser inst [] `shouldBe` return Nothing
      runParser inst [Ident "mov", RegToken 0, Digits 1] `shouldBe` return (Just (Reg 0 `Mov` word 1, []))

  describe "reg" $
    it "parses a register" $ do
      runParser reg [RegToken 0]             `shouldBe` return (Just (Reg 0, []))
      runParser reg [RegToken 0, RegToken 1] `shouldBe` return (Just (Reg 0, [RegToken 1]))

  describe "lex1" $
    it "lex a token" $ do
      runLexer lex1 ""          `shouldBe` return (Nothing)
      runLexer lex1 "R0"        `shouldBe` return (Just $ RegToken 0)
      runLexer lex1 " R1"       `shouldBe` return (Just $ RegToken 1)
      runLexer lex1 "R0 R1"     `shouldBe` return (Just $ RegToken 0)
      runLexer lex1 "mov R0 R1" `shouldBe` return (Just $ Ident "mov")

      runLexer lex1 "R255"  `shouldBe` return (Just $ RegToken 255)
      runLexer lex1 "R256"  `shouldSatisfy` isLeft
      runLexer lex1 "R2560" `shouldSatisfy` isLeft

      runLexer lex1 "R0a" `shouldSatisfy` isLeft
      runLexer lex1 "0a"  `shouldSatisfy` isLeft
      runLexer lex1 "R"   `shouldSatisfy` isLeft
      runLexer lex1 "R@"  `shouldSatisfy` isLeft

  describe "lexer" $
    it "lex tokens" $ do
      runLexer lexer ""          `shouldBe` return []
      runLexer lexer "R0"        `shouldBe` return [RegToken 0]
      runLexer lexer " R1"       `shouldBe` return [RegToken 1]
      runLexer lexer "R0 R1"     `shouldBe` return [RegToken 0, RegToken 1]
      runLexer lexer "mov R0 R1" `shouldBe` return [Ident "mov", RegToken 0, RegToken 1]
