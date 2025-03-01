-- HSpec tests for Val.hs
-- Execute: run haskell InterpretSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval
import Interpret

main :: IO ()
main = hspec $ do
  describe "evalF" $ do
    it "preserves output for numbers" $ do
        evalF ([], "x") (Real 3.0) `shouldBe` ([Real 3.0], "x")

    it "passes through operators" $ do
        evalF ([Real 2.2, Integer 2], "") (Id "*") `shouldBe` ([Real 4.4], "")

    it "propagates output" $ do
        evalF ([Integer 2], "") (Id ".") `shouldBe` ([],"2")    

  describe "interpret" $ do
    context "RPN" $ do
        it "multiplies two integers" $ do
            interpret "2 3 *" `shouldBe` ([Integer 6], "")      

        -- numerical precision makes this tricky
        it "multiplies floats and integers" $ do
            interpret "2 2.2 3.4 * *" `shouldBe` ([Real 14.960001], "")

    context "EMIT" $ do
        it "prints character from integer" $ do
            interpret "65 EMIT" `shouldBe` ([], "65 : A")
        it "prints character from real" $ do
            interpret "66.9 EMIT" `shouldBe` ([], "66 : B")
        it "errors on non-numeric" $ do
            evaluate (interpret "X EMIT") `shouldThrow` errorCall "Non-numeric argument to EMIT"

    context "CONCAT2" $ do
      it "concatenates two strings in reverse order" $ do
        interpret "Hello World CONCAT2" `shouldBe` ([], "HelloWorld")
        interpret "PLP ! CONCAT2" `shouldBe` ([], "PLP!")

      it "errors on non-string arguments" $ do
        evaluate (interpret "5 3 CONCAT2") `shouldThrow` errorCall "Type Mismatch"
        evaluate (interpret "3.14 \"x\" CONCAT2") `shouldThrow` errorCall "Type Mismatch"

      it "errors on insufficient stack elements" $ do
        evaluate (interpret "\"alone\" CONCAT2") `shouldThrow` errorCall "Stack underflow"
        evaluate (interpret "CONCAT2") `shouldThrow` errorCall "Stack underflow"

    context "CONCAT3" $ do
          it "concatenates two strings in reverse order" $ do
            interpret "Hello World ! CONCAT3" `shouldBe` ([], "HelloWorld!")
            interpret "! PLP Love CONCAT3" `shouldBe` ([], "!PLPLove")

          it "errors on non-string arguments" $ do
            evaluate (interpret "5 3 4 CONCAT3") `shouldThrow` errorCall "Type Mismatch"
            evaluate (interpret "3.14 x 5 CONCAT3") `shouldThrow` errorCall "Type Mismatch"

          it "errors on insufficient stack elements" $ do
            evaluate (interpret "alone CONCAT3") `shouldThrow` errorCall "Stack underflow"
            evaluate (interpret "CONCAT3") `shouldThrow` errorCall "Stack underflow"

    context "STR" $ do
        it "converts integer to string" $ do
            interpret "5 STR" `shouldBe` ([], "5")
        it "converts real to string" $ do
            interpret "3.14 STR" `shouldBe` ([], "3.14")
        it "leaves Id (string) unchanged" $ do
            interpret "hello STR" `shouldBe` ([], "\"hello\"")
        it "errors on empty stack" $ do
            evaluate (interpret "STR") `shouldThrow` errorCall "Stack underflow"

    context "CR" $ do
        it "prints newline" $ do
            interpret "CR" `shouldBe` ([], "\n")

    context "Printout" $ do
        it "computes product and outputs" $ do
            interpret "2 6 * ." `shouldBe` ([], "12")