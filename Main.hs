module Main where

--import UnsafeLog
import SExp
import CPS
import Machine
import GenSym

testProgram :: String
testProgram = "(define (fact x) (if (= x 0) 1 (* x (fact (- x 1))))) (fact 100)"

evalString :: String -> Value
evalString s = case parseSExps (mLex s) of
  Just exprs -> runGenSymState ((parseProgram exprs) >>= runMachine)
  Nothing -> error "Parse Error"


main :: IO ()
main = interact (show . evalString) >> putStr "\n"

