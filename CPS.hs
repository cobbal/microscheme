module CPS where
import SExp
import Data.List
import Control.Monad.State

data CExp = Atomic AExp
          | MApp AExp [AExp]
          | MIf AExp CExp CExp
          | MSet Identifier AExp AExp

data AExp = MNum Integer
          | MBool Bool
          | MId Identifier
          | MLambda [Identifier] CExp
          | MVoid

data CProg = CProg [CDef] CExp

instance Show CProg where
  show (CProg defs expr) = foldr (\a b -> a ++ "\n\n" ++ b) "" (map show defs) ++ show expr

data CDef = CDef Identifier AExp

instance Show CDef where
  show (CDef name value) = name ++ ": " ++ show value

instance Show CExp where
  show (Atomic a) = show a
  show (MApp fn args) = concat ["(", show (Atomic fn),
                               " ", intercalate " " (map show args), ")"]
  show (MIf val consequent alternate) = concat ["(_if ", show val,
                                                " ", show consequent,
                                                " ", show alternate, ")"]
  show (MSet ident value k) = concat ["(_set ", ident,
                                      " ", show value,
                                      " ",  show k, ")"]

instance Show AExp where
  show (MNum n) = show n
  show (MBool True) = "#t"
  show (MBool False) = "#f"
  show (MId i) = i
  show (MLambda formals expr) = concat ["(λ (", intercalate " " formals, ") ",
                                        show expr, ")"]
  show MVoid = "VOID"

sLambda :: [SExp] -> SExp -> SExp
sLambda formals expr = (SList [SId "lambda", SList formals, expr])

type GenSymState = State Integer
gensym :: String -> GenSymState Identifier
gensym sym = do
  s <- get
  put (s + 1)
  return (sym ++ "$" ++ show s)

cpsM :: SExp -> GenSymState AExp
cpsM (SId i) = return (MId i)
cpsM (SNumber n) = return (MNum n)
cpsM (SBool b) = return (MBool b)
cpsM (SList [SId "lambda", SList formals, expr]) = do
  k <- gensym "k"
  t <- cpsT expr (MId k)
  return (MLambda (expToIds formals ++ [k]) t)
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

cpsTk :: SExp -> (AExp -> CExp) -> GenSymState CExp
cpsTk expr@(SList (SId "lambda" : _)) kont = do
  m <- cpsM expr
  return (kont m)

cpsT :: SExp -> AExp -> GenSymState CExp
cpsT expr@(SList (SId "lambda" : _)) cont = do
  m <- cpsM expr
  return (MApp cont [m])
cpsT fullExp@(SList [SId "let", SList bindings, expr]) cont =
  let bindings' = map parseBinding bindings in
  cpsT (SList (sLambda (map fst bindings') expr : (map snd bindings'))) cont
  where parseBinding :: SExp -> (SExp, SExp)
        parseBinding (SList [i, e]) = (i, e)
        parseBinding _ = error $ "Error: funky let form: " ++ show fullExp
cpsT (SList [SId "if", condition, consequent, alternate]) cont = do
  v <- gensym "v"
  consequent' <- cpsT consequent cont
  alternate' <- cpsT alternate cont
  cpsT condition (MLambda [v] (MIf (MId v) consequent' alternate'))
cpsT (SList [SId "set!", SId i, val]) cont = do
  v <- gensym "v"
  cpsT val (MLambda [v] (MSet i (MId v) cont))
cpsT expr _
  | primSyntax expr = error $ "bad special form " ++ show expr
cpsT (SList (fn : args)) cont = do
  f <- gensym "f"
  t <- helper f (reverse args) [] -- reverse needed since we recur inside-out
  cpsT fn (MLambda [f] t)
  where helper :: Identifier -> [SExp] -> [AExp] -> GenSymState CExp
        helper f [] bound = return (MApp (MId f) (bound ++ [cont]))
        helper f (arg : args') bound = do
          e <- gensym "e"
          subexp <- helper f args' ((MId e) : bound)
          cpsT arg (MLambda [e] subexp)
cpsT expr cont = do
  m <- cpsM expr
  return (MApp cont [m])

parseProgram :: [SExp] -> CProg
parseProgram forms = runGenSymState (parseProgramWithState forms)
  where parseProgramWithState :: [SExp] -> GenSymState CProg
        parseProgramWithState [expr] = do
          m <- cpsT expr (MId "_halt")
          return (CProg [] m)
        parseProgramWithState (def : defs) = do
          CProg defs' expr <- parseProgramWithState defs
          def' <- parseDef def
          return (CProg (def' : defs') expr)
        parseProgramWithState [] = error "you forgot your program"

parseDef :: SExp -> GenSymState CDef
parseDef (SList [SId "define", SList (SId fn : formals), expr]) = do
  value <- cpsM (sLambda formals expr)
  return (CDef fn value)
parseDef _ = error "you defined a not defined thingy in the define thingy slot"

runGenSymState :: GenSymState a -> a
runGenSymState s = evalState s 0
