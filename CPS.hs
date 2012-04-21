module CPS where
import SExp
import Data.List
import GenSym

data CExp = Atomic AExp
          | App AExp [AExp]
          | If AExp CExp CExp
          | Set Identifier AExp AExp

data AExp = Num Integer
          | Bool Bool
          | Id Identifier
          | Lambda [Identifier] CExp
          | Void

data CProg = CProg [CDef] CExp

instance Show CProg where
  show (CProg defs expr) = foldr (\a b -> a ++ "\n\n" ++ b) "" (map show defs) ++ show expr

data CDef = CDef Identifier AExp

instance Show CDef where
  show (CDef name value) = name ++ ": " ++ show value

instance Show CExp where
  show (Atomic a) = show a
  show (App fn args) = concat ["(", show (Atomic fn),
                               " ", intercalate " " (map show args), ")"]
  show (If val consequent alternate) = concat ["(_if ", show val,
                                                " ", show consequent,
                                                " ", show alternate, ")"]
  show (Set ident value k) = concat ["(_set ", ident,
                                      " ", show value,
                                      " ",  show k, ")"]

instance Show AExp where
  show (Num n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Id i) = i
  show (Lambda formals expr) = concat ["(λ (", intercalate " " formals, ") ",
                                        show expr, ")"]
  show Void = "VOID"

sLambda :: [SExp] -> SExp -> SExp
sLambda formals expr = (SList [SId "lambda", SList formals, expr])

cpsM :: SExp -> GenSymState AExp
cpsM (SId i) = return (Id i)
cpsM (SNumber n) = return (Num n)
cpsM (SBool b) = return (Bool b)
cpsM (SList [SId "lambda", SList formals, expr]) = do
  k <- gensym "k"
  t <- cpsTc expr (Id k)
  return (Lambda (expToIds formals ++ [k]) t)
  where expToIds :: [SExp] -> [Identifier]
        expToIds [] = []
        expToIds (SId i : is) = i : expToIds is
        expToIds (subexp : _) = error ("lambda called with non-identifier " ++ show subexp)
cpsM f = error ("WHY ERROR? " ++ show f)

primSyntax :: SExp -> Bool
primSyntax (SList (SId "let" : _)) = True
primSyntax (SList (SId "if" : _)) = True
primSyntax (SList (SId "set!" : _)) = True
primSyntax _ = False


cpsTk :: SExp -> (AExp -> GenSymState CExp) -> GenSymState CExp
cpsTk expr@(SList (SId "lambda" : _)) kont = do
  m <- cpsM expr
  kont m
cpsTk fullExp@(SList _) kont = do
  rv <- gensym "rv"
  ik <- kont (Id rv)
  let cont = (Lambda [rv] ik) in
    cpsTc fullExp cont
cpsTk expr kont = do
  m <- cpsM expr
  kont m

cpsTc :: SExp -> AExp -> GenSymState CExp
cpsTc expr@(SList (SId "lambda" : _)) cont = do
  m <- cpsM expr
  return (App cont [m])
cpsTc fullExp@(SList [SId "let", SList bindings, expr]) cont =
  let bindings' = map parseBinding bindings in
  cpsTc (SList (sLambda (map fst bindings') expr : (map snd bindings'))) cont
  where parseBinding :: SExp -> (SExp, SExp)
        parseBinding (SList [i, e]) = (i, e)
        parseBinding _ = error $ "Error: funky let form: " ++ show fullExp
cpsTc (SList [SId "if", condition, consequent, alternate]) cont = do
  v <- gensym "v"
  consequent' <- cpsTc consequent cont
  alternate' <- cpsTc alternate cont
  cpsTc condition (Lambda [v] (If (Id v) consequent' alternate'))
cpsTc (SList [SId "set!", SId i, val]) cont = do
  v <- gensym "v"
  cpsTc val (Lambda [v] (Set i (Id v) cont))
cpsTc expr _
  | primSyntax expr = error $ "bad special form " ++ show expr
cpsTc (SList (fn : args)) cont = do
  -- reverse because recursion turns stuff inside out
  cpsTk fn (\df -> helper df args [])
  where helper :: AExp -> [SExp] -> [AExp] -> GenSymState CExp
        helper f [] bound = return (App f (bound ++ [cont]))
        helper f (arg : args') bound =
          cpsTk arg (\de -> helper f args' (bound ++ [de]))

cpsTc expr cont = do
  m <- cpsM expr
  return (App cont [m])

parseProgram :: [SExp] -> GenSymState CProg
parseProgram [expr] = do
  m <- cpsTc expr (Id "_halt")
  return (CProg [] m)
parseProgram (def : defs) = do
  CProg defs' expr <- parseProgram defs
  def' <- parseDef def
  return (CProg (def' : defs') expr)
parseProgram [] = error "you forgot your program"

parseDef :: SExp -> GenSymState CDef
parseDef (SList [SId "define", SList (SId fn : formals), expr]) = do
  value <- cpsM (sLambda formals expr)
  return (CDef fn value)
parseDef _ = error "you defined a not defined thingy in the define thingy slot"
