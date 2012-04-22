module CPS where

import qualified SExp
import SExp (SExp)
import Identifier (Identifier)
import Data.List (intercalate)
import GenSym (GenSymState, gensym)

data CExp = Atomic AExp
          | App AExp [AExp]
          | If AExp CExp CExp
          | Set Identifier AExp AExp

data AExp = Number Integer
          | Boolean Bool
          | Id Identifier
          | Lambda [Identifier] CExp
          | Void

instance Show CExp where
  show (Atomic a) = show a
  show (App fn args) = concat ["(", show (Atomic fn), " ", intercalate " " (map show args), ")"]
  show (If val consequent alternate) = concat ["(if ", show val, " ", show consequent,
                                                " ", show alternate, ")"]
  show (Set ident value k) = concat ["(set! ", ident, " ", show value, " ",  show k, ")"]

instance Show AExp where
  show (Number n) = show n
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Id i) = i
  show (Lambda formals expr) = concat ["(λ (", intercalate " " formals, ") ", show expr, ")"]
  show Void = "#<void>"

data Program = Program [Definition] CExp

instance Show Program where
  show (Program defs expr) = intercalate "\n\n" (map show defs ++ [show expr])

data Definition = Definition Identifier AExp

instance Show Definition where
  show (Definition name value) = name ++ ": " ++ show value

sLambda :: [SExp] -> SExp -> SExp
sLambda formals expr = (SExp.List [SExp.Id "lambda", SExp.List formals, expr])

cpsM :: SExp -> GenSymState AExp
cpsM (SExp.Id i) = return (Id i)
cpsM (SExp.Number n) = return (Number n)
cpsM (SExp.Boolean b) = return (Boolean b)
cpsM (SExp.List [SExp.Id "lambda", SExp.List formals, expr]) = do
  k <- gensym "k"
  t <- cpsTc expr (Id k)
  return (Lambda (expToIds formals ++ [k]) t)
  where expToIds :: [SExp] -> [Identifier]
        expToIds [] = []
        expToIds (SExp.Id i : is) = i : expToIds is
        expToIds (subexp : _) = error ("lambda formal isn't an identifier: " ++ show subexp)
cpsM f = error ("tried to convert non-atomic " ++ show f ++ " without a continuation")

isSyntax :: SExp -> Bool
isSyntax (SExp.List (SExp.Id "let" : _)) = True
isSyntax (SExp.List (SExp.Id "if" : _)) = True
isSyntax (SExp.List (SExp.Id "set!" : _)) = True
isSyntax _ = False

cpsTk :: SExp -> (AExp -> GenSymState CExp) -> GenSymState CExp
cpsTk expr@(SExp.List (SExp.Id "lambda" : _)) kont = cpsM expr >>= kont
cpsTk fullExp@(SExp.List _) kont = do
  rv <- gensym "rv"
  ik <- kont (Id rv)
  cpsTc fullExp (Lambda [rv] ik)
cpsTk expr kont = cpsM expr >>= kont

cpsTc :: SExp -> AExp -> GenSymState CExp
cpsTc expr@(SExp.List (SExp.Id "lambda" : _)) cont = do
  m <- cpsM expr
  return (App cont [m])
cpsTc fullExp@(SExp.List [SExp.Id "let", SExp.List bindings, expr]) cont =
  let bindings' = map parseBinding bindings in
  cpsTc (SExp.List (sLambda (map fst bindings') expr : (map snd bindings'))) cont
  where parseBinding :: SExp -> (SExp, SExp)
        parseBinding (SExp.List [i, e]) = (i, e)
        parseBinding _ = error ("funky let form: " ++ show fullExp)
cpsTc (SExp.List [SExp.Id "if", condition, consequent, alternate]) cont = do
  v <- gensym "v"
  consequent' <- cpsTc consequent cont
  alternate' <- cpsTc alternate cont
  cpsTc condition (Lambda [v] (If (Id v) consequent' alternate'))
cpsTc (SExp.List [SExp.Id "set!", SExp.Id i, val]) cont = do
  v <- gensym "v"
  cpsTc val (Lambda [v] (Set i (Id v) cont))
cpsTc expr _
  | isSyntax expr = error ("bad special form " ++ show expr)
cpsTc (SExp.List (fn : args)) cont = do
  cpsTk fn (\f' -> helper f' args [])
  where helper :: AExp -> [SExp] -> [AExp] -> GenSymState CExp
        helper f [] bound = return (App f (bound ++ [cont]))
        helper f (arg : args') bound =
          cpsTk arg (\e' -> helper f args' (bound ++ [e']))
cpsTc expr cont = do
  m <- cpsM expr
  return (App cont [m])

parseProgram :: [SExp] -> GenSymState Program
parseProgram [expr] = do
  m <- cpsTc expr (Id "_halt")
  return (Program [] m)
parseProgram (def : rest) = do
  Program defs expr <- parseProgram rest
  def' <- parseDef def
  return (Program (def' : defs) expr)
parseProgram [] = error "you forgot your program"

parseDef :: SExp -> GenSymState Definition
parseDef (SExp.List [SExp.Id "define", SExp.List (SExp.Id fn : formals), expr]) = do
  value <- cpsM (sLambda formals expr)
  return (Definition fn value)
parseDef s = error ("Expected definition, got " ++ show s)
