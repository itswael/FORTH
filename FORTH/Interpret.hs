-- Interpret.hs
module Interpret where

import Val
import Eval
import Flow
import qualified Data.Map as M

--type Env = M.Map String [String]
type State = ([Val], String, Env)

valToStr :: Val -> String
valToStr (Id s) = s
valToStr (Integer i) = show i
valToStr (Real r) = show r

-- Process tokens recursively to handle multi-token constructs
-- Interpret.hs
processTokens :: State -> [Val] -> State
processTokens state [] = state
processTokens state@(stack, out, env) (val:rest) =
  case val of
    Id ":" ->
      let (newState, remaining) = defineFunction state rest
      in processTokens newState remaining
    _      -> processTokens (evalF state val) rest

defineFunction :: State -> [Val] -> (State, [Val])
defineFunction (stack, out, env) tokens =
  case tokens of
    (Id name : rest) ->
      let (body, remaining) = break (== Id ";") rest
          funcBody = map valToStr body  -- Use valToStr to handle all Val types
          --funcBody = map (\(Id w) -> w) body  -- Extract function body
          newEnv = M.insert name funcBody env
      in if null remaining
          then error "Unterminated function definition"
          else ((stack, out, newEnv), drop 1 remaining)  -- Skip ";"
    _ -> error "Malformed function definition"

evalF :: State -> Val -> State
evalF (stack, out, env) (Id op) =
  let (newStack, newOut) = evalOut op env (stack, out)
  in (newStack, newOut, env)

evalF (stack, out, env) val = (val : stack, out, env)

interpret :: String -> ([Val], String)
interpret text =
  let tokens = map strToVal (words text)
      (finalStack, finalOut, _) = processTokens ([], "", M.empty) tokens
  in (finalStack, finalOut)