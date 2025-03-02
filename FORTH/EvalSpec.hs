module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval
import qualified Data.Map as M

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" M.empty [Integer 2, Integer 3] `shouldBe` [Integer 6]

        it "multiplies floats" $ do
            eval "*" M.empty [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" M.empty [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" M.empty [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "errors on too few arguments" $ do
            evaluate (eval "*" M.empty []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" M.empty [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "+" $ do
        it "adds integers" $ do
            eval "+" M.empty [Integer 2, Integer 3] `shouldBe` [Integer 5]

        it "adds floats" $ do
            eval "+" M.empty [Integer 2, Real 3.0] `shouldBe` [Real 5.0]
            eval "+" M.empty [Real 3.0, Integer 3] `shouldBe` [Real 6.0]
            eval "+" M.empty [Real 4.0, Real 3.0] `shouldBe` [Real 7.0]

        it "errors on too few arguments" $ do
            evaluate (eval "+" M.empty []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "+" M.empty [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "-" $ do
        it "subtracts integers" $ do
            eval "-" M.empty [Integer 2, Integer 3] `shouldBe` [Integer 1]

        it "subtracts floats" $ do
            eval "-" M.empty [Integer 2, Real 3.0] `shouldBe` [Real 1.0]
            eval "-" M.empty [Real 3.0, Integer 3] `shouldBe` [Real 0.0]
            eval "-" M.empty [Real 3.0, Real 4.0] `shouldBe` [Real 1.0]

        it "errors on too few arguments" $ do
            evaluate (eval "-" M.empty []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "-" M.empty [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "/" $ do
        it "divides integers" $ do
            eval "/" M.empty [Integer 3, Integer 2] `shouldBe` [Integer 0]

        it "divides floats" $ do
            eval "/" M.empty [Integer 2, Real 3.0] `shouldBe` [Real 1.5]
            eval "/" M.empty [Real 3.0, Integer 3] `shouldBe` [Real 1.0]
            eval "/" M.empty [Real 3.0, Real 4.0] `shouldBe` [Real (4.0/3.0)]

        it "errors on too few arguments" $ do
            evaluate (eval "/" M.empty []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "/" M.empty [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "^" $ do
        it "powers integers" $ do
            eval "^" M.empty [Integer 3, Integer 2] `shouldBe` [Integer 8]

        it "powers floats" $ do
            eval "^" M.empty [Integer 3, Real 2.0] `shouldBe` [Real 8.0]
            eval "^" M.empty [Real 3.0, Integer 3] `shouldBe` [Real 27.0]
            eval "^" M.empty [Real 4.0, Real 3.0] `shouldBe` [Real 81.0]

        it "errors on too few arguments" $ do
            evaluate (eval "^" M.empty []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "^" M.empty [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" M.empty [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" M.empty [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" M.empty [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" M.empty []) `shouldThrow` errorCall "Stack underflow"

    context "User-Defined Functions" $ do
        let env = M.fromList [("SQUARE", ["DUP", "*"])]
        it "evaluates SQUARE function" $ do
            eval "SQUARE" env [Integer 4] `shouldBe` [Integer 16]
            eval "SQUARE" env [Real 3.0] `shouldBe` [Real 9.0]

  describe "evalOut" $ do
      context "." $ do
        it "prints top of stack" $ do
            evalOut "." M.empty ([Id "x"], "") `shouldBe` ([], "x")
            evalOut "." M.empty ([Integer 2], "") `shouldBe` ([], "2")
            evalOut "." M.empty ([Real 2.2], "") `shouldBe` ([], "2.2")

        it "errors on empty stack" $ do
            evaluate (evalOut "." M.empty ([], "")) `shouldThrow` errorCall "Stack underflow"

      context "EMIT" $ do
        it "prints character from integer" $ do
            evalOut "EMIT" M.empty ([Integer 65], "") `shouldBe` ([], "A")
        it "prints character from real (floor)" $ do
            evalOut "EMIT" M.empty ([Real 66.9], "") `shouldBe` ([], "B")
        it "errors on non-numeric argument" $ do
            evaluate (evalOut "EMIT" M.empty ([Id "X"], "")) `shouldThrow` errorCall "Non-numeric argument to EMIT"
        it "errors on empty stack" $ do
            evaluate (evalOut "EMIT" M.empty ([], "")) `shouldThrow` errorCall "Stack underflow"

      context "CONCAT2" $ do
        it "concatenates two Id (string) elements in reverse order" $ do
            evalOut "CONCAT2" M.empty ([Id "Hello", Id "World"], "") `shouldBe` ([], "WorldHello")
        it "errors on non-Id arguments" $ do
            evaluate (evalOut "CONCAT2" M.empty ([Id "x", Integer 42], "")) `shouldThrow` errorCall "Type Mismatch"
            evaluate (evalOut "CONCAT2" M.empty ([Real 3.14, Id "y"], "")) `shouldThrow` errorCall "Type Mismatch"
        it "errors on stack underflow" $ do
            evaluate (evalOut "CONCAT2" M.empty ([Id "x"], "")) `shouldThrow` errorCall "Stack underflow"
            evaluate (evalOut "CONCAT2" M.empty ([], "")) `shouldThrow` errorCall "Stack underflow"

      context "CONCAT3" $ do
        it "concatenates three Id (string) elements in reverse order" $ do
            evalOut "CONCAT3" M.empty ([Id "A", Id "B", Id "C"], "") `shouldBe` ([], "CBA")
        it "errors on non-Id arguments" $ do
            evaluate (evalOut "CONCAT3" M.empty ([Integer 1, Id "B", Id "C"], "")) `shouldThrow` errorCall "Type Mismatch"
        it "errors on stack underflow" $ do
            evaluate (evalOut "CONCAT3" M.empty ([Id "A", Id "B"], "")) `shouldThrow` errorCall "Stack underflow"

      context "STR" $ do
        it "converts integer to string" $ do
            evalOut "STR" M.empty ([Integer 5], "") `shouldBe` ([], "5")
        it "converts real to string" $ do
            evalOut "STR" M.empty ([Real 3.14], "") `shouldBe` ([], "3.14")
        it "leaves Id (string) unchanged" $ do
            evalOut "STR" M.empty ([Id "hello"], "") `shouldBe` ([], "hello")
        it "errors on empty stack" $ do
            evaluate (evalOut "STR" M.empty ([], "")) `shouldThrow` errorCall "Stack underflow"

      context "CR" $ do
        it "adds newline to output" $ do
            evalOut "CR" M.empty ([], "output") `shouldBe` ([], "output\n")

      context "General case" $ do
        it "passes through unrecognized operators" $ do
            evalOut "UNKNOWN" M.empty ([Integer 1], "out") `shouldBe` ([Id "UNKNOWN", Integer 1], "out")