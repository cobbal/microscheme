module Main where

--import UnsafeLog
import SExp
import CPS
import Machine

testProgram :: String
testProgram = "(define (fact x) (if (= x 0) 1 (* x (fact (- x 1))))) (fact 100)"

evalString :: String -> Value
evalString s = case parseSExps (mLex s) of
  Just exprs -> let prog = (parseProgram exprs) in runMachine prog
  Nothing -> error "Parse Error"


main :: IO ()
main = interact (show . evalString) >> putStr "\n"

