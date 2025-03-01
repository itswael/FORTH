module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Char (chr)

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]

-- 1. Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")

-- 2. Addition
-- if arguments are integers, keep result as integer
eval "+" (Integer x: Integer y:tl) = Integer (x+y) : tl
-- if any argument is float, make result a float
eval "+" (x:y:tl) = (Real $ toFloat x + toFloat y) : tl
-- any remaining cases are stacks too short
eval "+" _ = error("Stack underflow")

-- 3. Subtraction
eval "-" (Integer x: Integer y:tl) = Integer (y-x) : tl
eval "-" (x:y:tl) = (Real $ toFloat y - toFloat x) : tl
eval "-" _ = error("Stack underflow")

-- 4. Division
eval "/" (Integer x: Integer y:tl) = Integer (y `div` x) : tl
eval "/" (x:y:tl) = (Real $ toFloat y / toFloat x) : tl
eval "/" _ = error("Stack underflow")

-- 5. Power
eval "^" (Integer x: Integer y:tl) = Integer (y^x) : tl
eval "^" (x:y:tl) = (Real $ toFloat y ** toFloat x) : tl
eval "^" _ = error("Stack underflow")

-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"

-- EMIT: print character from ASCII code
evalOut "EMIT" (x:tl, out) = case x of
    Integer i -> (tl, out ++ (show i) ++ " : " ++ [chr i])
    Real r    -> (tl, out ++ (show (fromIntegral (floor r))) ++ " : " ++ [chr (fromIntegral (floor r))])
    _         -> error "Non-numeric argument to EMIT"
evalOut "EMIT" ([], _) = error "Stack underflow"

-- CONCAT2: concatenate 2 strings
evalOut "CONCAT2" (x:y:tl, out) = case (x, y) of
    (Id a, Id b) -> (tl, out ++ b ++ a)
    _            -> error "Type Mismatch"
evalOut "CONCAT2" (_, _) = error "Stack underflow"

-- CONCAT3: concatenate 3 strings
evalOut "CONCAT3" (x:y:z:tl, out) = case (x, y, z) of
    (Id a, Id b, Id c) -> (tl, out ++ c ++ b ++ a)
    _            -> error "Type Mismatch"
evalOut "CONCAT3" (_, _) = error "Stack underflow"

-- STR: print string
evalOut "STR" (x:tl, out) = case x of
    Integer i -> (tl, out ++ (show i))
    Real r    -> (tl, out ++ (show r))
    Id s      -> (tl, out ++ (show s))
evalOut "STR" ([], _) = error "Stack underflow"

-- handle CR to add newline to output
evalOut "CR" (stack, out) = (stack, out ++ "\n")

-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)