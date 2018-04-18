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

      mainParser "mov R0 R1 ;"  `shouldBe` prog [Reg 0 `Mov` reg 1]
      mainParser "mov R0 R1 ;;" `shouldBe` prog [Reg 0 `Mov` reg 1]

      mainParser "not R1 128 ; not R8 7" `shouldBe` prog [Reg 1 `Not` word 128, Reg 8 `Not` word 7]
      mainParser "mov R1 128 ; mov R8 7" `shouldBe` prog [Reg 1 `Mov` word 128, Reg 8 `Mov` word 7]

      mainParser "mov R1 128 \n mov R8 7"    `shouldBe` prog [Reg 1 `Mov` word 128, Reg 8 `Mov` word 7]
      mainParser "mov R1 128 \n \n mov R8 7" `shouldBe` prog [Reg 1 `Mov` word 128, Reg 8 `Mov` word 7]

      mainParser "mov"          `shouldSatisfy` isLeft
      mainParser "mov ; R0 R1"  `shouldSatisfy` isLeft

      mainParser "%"                       `shouldBe` prog []
      mainParser "mov R1 0 %"              `shouldBe` prog [Reg 1 `Mov` word 0]
      mainParser "mov R1 0 % \n"           `shouldBe` prog [Reg 1 `Mov` word 0]
      mainParser "mov R1 0 % ; ; not R0 1" `shouldBe` prog [Reg 1 `Mov` word 0, Reg 0 `Not` word 1]

      mainParser " ; ;  mov R1 0 %  ; ; \n %" `shouldBe` prog [Reg 1 `Mov` word 0]

  describe "reg" $
    it "parses a register" $ do
      run reg "R0"          `shouldBe` return (Reg 0)
      run reg "R1"          `shouldBe` return (Reg 1)
      run reg "R99"         `shouldBe` return (Reg 99)
      run reg "R4294967296" `shouldBe` return (Reg 4294967296)

      run reg "R0 x"  `shouldBe` return (Reg 0)
      run reg "R1 R0" `shouldBe` return (Reg 1)

      run reg "R"   `shouldSatisfy` isLeft
      run reg "R00" `shouldSatisfy` isLeft
      run reg "R08" `shouldSatisfy` isLeft
      run reg "a"   `shouldSatisfy` isLeft
      run reg ""    `shouldSatisfy` isLeft
      run reg "R 0" `shouldSatisfy` isLeft

  describe "operand" $
    it "parses an operand" $ do
      let reg = Register . Reg
      let word = Value . Word

      run operand "R0"          `shouldBe` return (reg 0)
      run operand "R1"          `shouldBe` return (reg 1)
      run operand "R99"         `shouldBe` return (reg 99)
      run operand "R4294967296" `shouldBe` return (reg 4294967296)

      run operand "0"          `shouldBe` return (word 0)
      run operand "1"          `shouldBe` return (word 1)
      run operand "99"         `shouldBe` return (word 99)
      run operand "4294967295" `shouldBe` return (word 4294967295)

      run operand "R0 x"  `shouldBe` return (reg 0)
      run operand "R1 R0" `shouldBe` return (reg 1)

      run operand "0 x" `shouldBe` return (word 0)
      run operand "1 0" `shouldBe` return (word 1)

      run operand "R"   `shouldSatisfy` isLeft
      run operand "R00" `shouldSatisfy` isLeft
      run operand "R08" `shouldSatisfy` isLeft
      run operand "a"   `shouldSatisfy` isLeft
      run operand ""    `shouldSatisfy` isLeft
      run operand "R 0" `shouldSatisfy` isLeft

      run operand "00" `shouldSatisfy` isLeft
      run operand "08" `shouldSatisfy` isLeft
      run operand "a"  `shouldSatisfy` isLeft
      run operand ""   `shouldSatisfy` isLeft
      run operand " 0" `shouldSatisfy` isLeft

  describe "inst" $
    it "parses an instruction" $ do
      let reg = Register . Reg
      let word = Value . Word

      run inst "mov R0 R1" `shouldBe` return (Reg 0 `Mov` reg 1)

      run inst "movR0 R1" `shouldSatisfy` isLeft
      run inst "mov R0"   `shouldSatisfy` isLeft
      run inst "mov"      `shouldSatisfy` isLeft

  describe "commentSep" $
    it "parses a commentSep" $ do
      run commentSep ";"                      `shouldSatisfy` isRight
      run commentSep "%;"                     `shouldSatisfy` isRight
      run commentSep "% ;"                    `shouldSatisfy` isRight
      run commentSep "% abc ;"                `shouldSatisfy` isRight
      run commentSep "% mov R0 1 ;"           `shouldSatisfy` isRight
      run commentSep "% mov 1 ;"              `shouldSatisfy` isRight
      run commentSep "% mov 1 ; % mov \n r ;" `shouldSatisfy` isRight
