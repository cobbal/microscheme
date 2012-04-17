module Main where

import Control.Monad.State
import SExp

data CExp = AExp AExp
          | MApp AExp AExp AExp
          | MCont AExp AExp

instance Show CExp where
  show (AExp (MNum n)) = show n
  show (AExp (MBool True)) = "#t"
  show (AExp (MBool False)) = "#f"
  show (AExp (MId i)) = i
  show (AExp (MLambda i expr)) = "(λ (" ++ i ++ ") " ++ show expr ++ ")"
  show (MApp fn arg cont) = "(" ++ show (AExp fn) ++
                            " " ++ show (AExp arg) ++
                            " " ++ show (AExp cont) ++ ")"
  show (MCont cont arg) = "(" ++ show (AExp cont) ++
                          " " ++ show (AExp arg) ++ ")"


data AExp = MNum Integer
          | MBool Bool
          | MId Identifier
          | MLambda Identifier CExp

sLambda :: [SExp] -> SExp -> SExp
sLambda formals expr = (SList [SId "lambda", SList formals, expr])

type GenSymState = State Integer
gensym :: GenSymState Identifier
gensym = do
  s <- get
  put (s + 1)
  return ("$" ++ show s)

cpsM :: SExp -> GenSymState AExp
cpsM (SId i) = return (MId i)
cpsM (SNumber n) = return (MNum n)
cpsM (SBool b) = return (MBool b)
cpsM (SList [SId "lambda", SList [SId formal], expr]) = do
  k <- gensym
  t <- cpsT expr (MId k)
  return (MLambda formal (AExp (MLambda k t)))
cpsM (SList [SId "lambda", SList (f : fs), expr]) =
  cpsM (sLambda [f] (sLambda fs expr))
cpsM _ = error ""

cpsT :: SExp -> AExp -> GenSymState CExp
cpsT expr@(SList (SId "lambda" : _)) cont = do
  m <- cpsM expr
  return (MCont cont m)
cpsT (SList [fn, arg]) cont = do
  f <- gensym
  e <- gensym
  t <- cpsT arg (MLambda e (MApp (MId f) (MId e) cont))
  cpsT fn (MLambda f t)
cpsT (SList (fn : arg : args)) cont =
  cpsT (SList ((SList [fn, arg]) : args)) cont
cpsT expr cont = do
  m <- cpsM expr
  return (MCont cont m)

runGenSymState :: GenSymState a -> a
runGenSymState s = evalState s 0

testProgram :: String
testProgram = "(define (fact x) (if (= x 0) 1 (* x (- x 1)))) (fact 100) (g a)"

main :: IO ()
main = case parseSExps (mLex testProgram) of
  Just exprs -> do
    let runExp expr = cpsT expr (MId "halt") in
      mapM_ (putStrLn . show) (evalState (mapM runExp exprs) 0)
  Nothing -> putStrLn "Parse Error"

