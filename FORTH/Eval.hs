module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Char (chr)
import qualified Data.Map as M

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> Env -> [Val] -> [Val]
eval op env stack
  | Just body <- M.lookup op env =  -- User-defined function
      foldl (\stk cmd -> eval cmd env stk) stack body
  | otherwise = case op of
      "*" -> multiplication stack
      "+" -> addition stack
      "-" -> subtraction stack
      "/" -> division stack
      "^" -> power stack
      "DUP" -> duplicate stack
      _ -> case strToVal op of
        Integer i -> Integer i : stack
        Real r -> Real r : stack
        Id s -> Id s : stack  -- Fallback: push unrecognized op as Id

---- 1. Multiplication
multiplication :: [Val] -> [Val]
multiplication (Integer x : Integer y : tl) = Integer (x * y) : tl
multiplication (x : y : tl) = Real (toFloat x * toFloat y) : tl
multiplication _ = error "Stack underflow"

---- 2. Addition
addition :: [Val] -> [Val]
addition (Integer x : Integer y : tl) = Integer (x + y) : tl
addition (x : y : tl) = Real (toFloat x + toFloat y) : tl
addition _ = error "Stack underflow"

---- 3. Subtraction
subtraction :: [Val] -> [Val]
subtraction (Integer x : Integer y : tl) = Integer (y - x) : tl
subtraction (x : y : tl) = Real (toFloat y - toFloat x) : tl
subtraction _ = error "Stack underflow"

--
---- 4. Division
division :: [Val] -> [Val]
division (Integer x : Integer y : tl) = Integer (y `div` x) : tl
division (x : y : tl) = Real (toFloat y / toFloat x) : tl
division _ = error "Stack underflow"

---- 5. Power
power :: [Val] -> [Val]
power (Integer x : Integer y : tl) = Integer (y ^ x) : tl
power (x : y : tl) = Real (toFloat y ** toFloat x) : tl
power _ = error "Stack underflow"

-- Duplicate the element at the top of the stack
duplicate :: [Val] -> [Val]
duplicate (x:tl) = x : x : tl
duplicate _ = error "Stack underflow"

evalOut :: String -> Env -> ([Val], String) -> ([Val], String)
-- Handle "." (print)
evalOut "." env (x:tl, out) = case x of
    Id s      -> (tl, out ++ s)  -- Print and pop
    Integer i -> (tl, out ++ show i)
    Real r    -> (tl, out ++ show r)
evalOut "." _ ([], _) = error "Stack underflow"

-- Handle "EMIT"
evalOut "EMIT" env (x:tl, out) = case x of
    Integer i -> (tl, out ++ [chr i])
    Real r    -> (tl, out ++ [chr (floor r)])
    _         -> error "Non-numeric argument to EMIT"
evalOut "EMIT" _ ([], _) = error "Stack underflow"

-- Handle "CONCAT2"
evalOut "CONCAT2" env (x:y:tl, out) = case (x, y) of
    (Id a, Id b) -> (tl, out ++ b ++ a)
    _            -> error "Type Mismatch"
evalOut "CONCAT2" _ (_, _) = error "Stack underflow"

-- Handle "CONCAT3"
evalOut "CONCAT3" env (x:y:z:tl, out) = case (x, y, z) of
    (Id a, Id b, Id c) -> (tl, out ++ c ++ b ++ a)
    _            -> error "Type Mismatch"
evalOut "CONCAT3" _ (_, _) = error "Stack underflow"

-- Handle "STR"
evalOut "STR" env (x:tl, out) = case x of
    Integer i -> (tl, out ++ show i)
    Real r    -> (tl, out ++ show r)
    Id s      -> (tl, out ++ s)
evalOut "STR" _ ([], _) = error "Stack underflow"

-- handle CR to add newline to output
evalOut "CR" env (stack, out) = (stack, out ++ "\n")

-- General case (pass env to eval)
evalOut op env (stack, out) = (eval op env stack, out)