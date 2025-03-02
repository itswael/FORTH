module Main where

-- Running: runhaskell Main.hs path_to_test_file

import Interpret
import System.Environment
import Val (Val(..))  -- Import the Val module to bring Id into scope

main :: IO ()
main = do
    (fileName:tl) <- getArgs
    contents <- readFile fileName
    let (stack, output) = interpret contents
    if null stack
        then
            putStrLn output
        else putStrLn $ "Stack is not empty: " ++ show stack