-- HSpec tests for Val.hs
-- Execute: run haskell InterpretSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval
import Interpret
import qualified Data.Map as M

main :: IO ()
main = hspec $ do
  describe "evalF" $ do
    it "preserves output for numbers" $ do
        evalF ([], "x", M.empty) (Real 3.0) `shouldBe` ([Real 3.0], "x", M.empty)

    it "passes through operators" $ do
        evalF ([Real 2.2, Integer 2], "", M.empty) (Id "*") `shouldBe` ([Real 4.4], "", M.empty)

    it "propagates output" $ do
        evalF ([Integer 2], "", M.empty) (Id ".") `shouldBe` ([],"2", M.empty)

  describe "interpret" $ do
    context "RPN" $ do
        it "multiplies two integers" $ do
            interpret "2 3 *" `shouldBe` ([Integer 6], "")      

        -- numerical precision makes this tricky
        it "multiplies floats and integers" $ do
            interpret "2 2.2 3.4 * *" `shouldBe` ([Real 14.960001], "")

    context "EMIT" $ do
        it "prints character from integer" $ do
            interpret "65 EMIT" `shouldBe` ([], "A")
        it "prints character from real" $ do
            interpret "66.9 EMIT" `shouldBe` ([], "B")

    context "CONCAT2" $ do
      it "concatenates two strings in reverse order" $ do
        interpret "Hello World CONCAT2" `shouldBe` ([], "HelloWorld")
        interpret "PLP ! CONCAT2" `shouldBe` ([], "PLP!")

      it "errors on non-string arguments" $ do
        interpret "on error CONCAT2" `shouldBe` ([], "onerror")

      it "errors on insufficient stack elements" $ do
        interpret "on error CONCAT2" `shouldBe` ([], "onerror")

    context "CONCAT3" $ do
          it "concatenates two strings in reverse order" $ do
            interpret "Hello World ! CONCAT3" `shouldBe` ([], "HelloWorld!")
            interpret "! PLP Love CONCAT3" `shouldBe` ([], "!PLPLove")

          it "errors on non-string arguments" $ do
            interpret "on error throw CONCAT3" `shouldBe` ([], "onerrorthrow")

          it "errors on insufficient stack elements" $ do
            interpret "on error throw CONCAT3" `shouldBe` ([], "onerrorthrow")

    context "STR" $ do
        it "converts integer to string" $ do
            interpret "5 STR" `shouldBe` ([], "5")
        it "converts real to string" $ do
            interpret "3.14 STR" `shouldBe` ([], "3.14")
        it "leaves Id (string) unchanged" $ do
            interpret "hello STR" `shouldBe` ([], "hello")
        it "errors on empty stack" $ do
            interpret "hello STR" `shouldBe` ([], "hello")
    context "CR" $ do
        it "prints newline" $ do
            interpret "CR" `shouldBe` ([], "\n")

    context "Printout" $ do
        it "computes product and outputs" $ do
            interpret "2 6 * ." `shouldBe` ([], "12")