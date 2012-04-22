module Main where

import SExp (parseSExps)
import CPS (parseProgram)
import Machine (Value, runMachine)
import Token (tokenize)
import GenSym (runGenSymState)

evalString :: String -> Value
evalString s = case parseSExps (tokenize s) of
  Just exprs -> runGenSymState (parseProgram exprs >>= runMachine)
  Nothing -> error "Parse Error"

main :: IO ()
main = interact (show . evalString) >> putStr "\n"
