module ELT0.ParserSpec where

import Test.Hspec

import Data.Either

import ELT0.Parser
import ELT0.Program

spec :: Spec
spec = do
  describe "insts" $
    it "parses instructions" $ do
      let reg = Register . Reg
      let word = Value . Word
      let prog = return . Program

      mainParser ""               `shouldBe` prog []
      mainParser " "              `shouldBe` prog []
      mainParser "mov R0 R1"      `shouldBe` prog [Reg 0 `Mov` reg 1]
      mainParser "mov R128 R100"  `shouldBe` prog [Reg 128 `Mov` reg 100]
      mainParser "add R0 1 2"     `shouldBe` prog [Add (Reg 0) (Value $ Word 1) (Value $ Word 2)]
      mainParser "add R0 1 R2"    `shouldBe` prog [Add (Reg 0) (Value $ Word 1) (reg 2)]
      mainParser "sub R1 R4 R9"   `shouldBe` prog [Sub (Reg 1) (reg 4) (reg 9)]
      mainParser "and R1 R4 R9"   `shouldBe` prog [And (Reg 1) (reg 4) (reg 9)]
      mainParser "or R1 128 R128" `shouldBe` prog [Or (Reg 1) (word 128) (reg 128)]
      mainParser "not R1 128"     `shouldBe` prog [Reg 1 `Not` word 128]
      mainParser "shl R2 R10 R8"  `shouldBe` prog [Shl (Reg 2) (reg 10) (reg 8)]
      mainParser "shr R2 R10 R8"  `shouldBe` prog [Shr (Reg 2) (reg 10) (reg 8)]

      mainParser "mov R0 R1 \n"   `shouldBe` prog [Reg 0 `Mov` reg 1]
      mainParser "mov R0 R1 \n\n" `shouldBe` prog [Reg 0 `Mov` reg 1]

      mainParser "not R1 128 \n not R8 7" `shouldBe` prog [Reg 1 `Not` word 128, Reg 8 `Not` word 7]
      mainParser "mov R1 128 \n mov R8 7" `shouldBe` prog [Reg 1 `Mov` word 128, Reg 8 `Mov` word 7]

      mainParser "mov R1 128 \n \n mov R8 7" `shouldBe` prog [Reg 1 `Mov` word 128, Reg 8 `Mov` word 7]

      mainParser "mov"          `shouldSatisfy` isLeft
      mainParser "mov \n R0 R1" `shouldSatisfy` isLeft

      mainParser "%"                         `shouldBe` prog []
      mainParser "mov R1 0 %"                `shouldBe` prog [Reg 1 `Mov` word 0]
      mainParser "mov R1 0 % \n"             `shouldBe` prog [Reg 1 `Mov` word 0]
      mainParser "mov R1 0 % \n \n not R0 1" `shouldBe` prog [Reg 1 `Mov` word 0, Reg 0 `Not` word 1]

      mainParser " \n \n  mov R1 0 %  \n \n \n %" `shouldBe` prog [Reg 1 `Mov` word 0]

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
