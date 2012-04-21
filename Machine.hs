module Machine where

import Data.List (intercalate)
import SExp
import GenSym
import qualified CPS
import UnsafeLog

import qualified Data.Map as Map
import Data.Map ((!))

data AExp = ExpLambda Lambda
          | Id Identifier
          | Val Value

data CExp = Atomic AExp
         | App AExp [AExp]
         | If AExp CExp CExp
         | Set Identifier AExp AExp

data Lambda = Lambda [Identifier] CExp

data Value = Closure Lambda Env
           | Bool Bool
           | Num Integer
           | Void
           | PrimHalt
           | PrimAdd
           | PrimSub
           | PrimMult
           | PrimEq
           | PrimVoid
           | PrimCallCC
           | PrimBegin

instance Show AExp where
  show (ExpLambda (Lambda args expr)) = "(λ (" ++ intercalate " " args ++ ") "
                                        ++ show expr ++ ")"
  show (Val v) = show v
  show (Id i) = i

instance Show CExp where
  show (Atomic a) = show a
  show (App fn args) = "(" ++ intercalate " " (map show (fn : args)) ++ ")"
  show (If a b c) = "(" ++ intercalate " " ["_if", show a, show b, show c] ++ ")"
  show (Set ident val k) = "(_set! " ++ ident ++ " " ++ show val ++ " " ++ show k ++ ")"

instance Show Value where
  -- show (Closure _ _) = "<closure>"
  show (Closure l _) = "closure {" ++ show (ExpLambda l) ++ "}"
  show (Bool b) = show b
  show (Num i) = show i
  show Void = "VOID"
  show PrimHalt = "_halt"
  show PrimAdd = "_+"
  show PrimSub = "_-"
  show PrimMult = "_*"
  show PrimEq = "_="
  show PrimVoid = "_void"
  show PrimCallCC = "_call/cc"
  show PrimBegin = "_begin"

instance Eq Value where
  (Bool a) == (Bool b) = (a == b)
  (Num a) == (Num b) = (a == b)
  _ == _ = False

type Env = Map.Map Identifier StoreAddress

type StoreAddress = Identifier
type Store = Map.Map StoreAddress Value

data Machine = CES CExp Env Store
             | Halt Value

instance Show Machine where
  show (CES state env store) = "{\n  " ++ show state ++ "\n  " ++
                              (intercalate ", " $ map formatMap (Map.keys env)) ++ "\n}"
    where formatMap :: Identifier -> String
          formatMap key = show key ++ " -> " ++ show (store ! (env ! key))
  show (Halt val) = "{" ++ show val ++ "}"

valueOf :: Env -> Store -> AExp -> Value
valueOf env store (Id ident)
  | ident `Map.member` env = store ! (env ! ident)
  | otherwise = error ("symbol " ++ show ident ++ " not found")
valueOf _ _ (Val v) = v
valueOf env _ (ExpLambda l) = Closure l env

updateEnvAndStore :: Env -> Store -> [Identifier] -> [Value] -> GenSymState (Env, Store)
updateEnvAndStore env store [] [] = return (env, store)
updateEnvAndStore env store (i : is) (v : vs) = do
  loc <- gensym "loc"
  (env', store') <- updateEnvAndStore env store is vs
  return (Map.insert i loc env', Map.insert loc v store')
updateEnvAndStore _ _ is vs = error $ concat ["arity error (expected ", show is,
                                              ", but got ", show vs, ")"]

performSets :: Env -> Store -> [(Identifier, Value)] -> Store
performSets _ store [] = store
performSets env store ((ident, val) : rest)
  | ident `Map.member` env = Map.insert (env ! ident) val (performSets env store rest)
  | otherwise = error $ "Tried to set " ++ ident ++ ", but it hasn't been bound"

step :: Machine -> GenSymState Machine
step (Halt v) = return (Halt v)
step (CES (Atomic a) env store) = return (Halt (valueOf env store a))
step (CES (App fn args) env store) =
  case valueOf env store fn of
    PrimHalt -> case args' of
      [v] -> return (Halt v)
      _ -> error "wrong number of args to PrimHalt"
    PrimVoid -> return (CES (App (last args) [Val Void]) env store)
    PrimAdd -> case (args, args') of
      ([_, _, k], [Num a, Num b, _]) -> return (CES (App k [Val (Num (a + b))]) env store)
      _ -> error "wrong number/type of args to +"
    PrimSub -> case (args, args') of
      ([_, _, k], [Num a, Num b, _]) -> return (CES (App k [Val (Num (a - b))]) env store)
      _ -> error "wrong number/type of args to -"
    PrimMult -> case (args, args') of
      ([_, _, k], [Num a, Num b, _]) -> return (CES (App k [Val (Num (a * b))]) env store)
      _ -> error "wrong number/type of args to *"
    PrimEq -> case (args, args') of
      ([_, _, k], [a, b, _]) -> return (CES (App k [Val (Bool (a == b))]) env store)
      _ -> error "wrong number of args to ="
    PrimCallCC -> case args of
      [target, k] -> do
        unusedCont <- gensym "unused"
        v <- gensym "v"
        {-
           when called as a function, the continuation is going to have an extra argument
           passed to it, the unused continuation. We need to explicitly eat that to avoid
           an arity error
        -}
        let k2arg = Lambda [v, unusedCont] (App k [Id v])
        return (CES (App target [ExpLambda k2arg, k]) env store)
      _ -> error $ "Error: wrong type arg to call/cc (" ++ show args' ++ ")"
    PrimBegin -> case reverse args of
      (cont : important : _) -> return (CES (App cont [important]) env store)
      [cont] -> return (CES (App cont [Val Void]) env store)
      [] -> error "huh?"
    Closure (Lambda formals body) clEnv -> do
        (env', store') <- updateEnvAndStore clEnv store formals args'
        return (CES body env' store')
    result -> error $ "Error: application of non-(closure/primitive) " ++ show result
  where args' = map (valueOf env store) args
step (CES (If condition consequent alternate) env store) = do
  case valueOf env store condition of
    Bool False -> return (CES alternate env store)
    _ -> return (CES consequent env store)
step (CES (Set ident value k) env store) =
  let store' = performSets env store [(ident, valueOf env store value)] in do
    return (CES (App k [Val Void]) env store')

stepUntilHalt :: Machine -> GenSymState Value
stepUntilHalt (Halt v) = return v
-- stepUntilHalt m = step m >>= stepUntilHalt
stepUntilHalt m = step (unsafeLog m) >>= stepUntilHalt

class FromCPS a b where
  fromCPS :: a -> b

instance FromCPS CPS.CExp CExp where
  fromCPS (CPS.Atomic a) = Atomic (fromCPS a)
  fromCPS (CPS.App fn args) = App (fromCPS fn) (map fromCPS args)
  fromCPS (CPS.If a b c) = If (fromCPS a) (fromCPS b) (fromCPS c)
  fromCPS (CPS.Set ident val cont) = Set ident (fromCPS val) (fromCPS cont)

instance FromCPS CPS.AExp AExp where
  fromCPS (CPS.Num n) = Val (Num n)
  fromCPS (CPS.Bool b) = Val (Bool b)
  fromCPS (CPS.Void) = Val Void
  fromCPS (CPS.Id ident) = Id ident
  fromCPS l@(CPS.Lambda _ _) = ExpLambda (fromCPS l)

instance FromCPS CPS.AExp Lambda where
  fromCPS (CPS.Lambda formals cont) = (Lambda formals (fromCPS cont))
  fromCPS _ = error "Tried to convert non-lambda into lambda"

defToPair :: Env -> CPS.CDef -> (Identifier, Value)
defToPair env (CPS.CDef ident l@(CPS.Lambda _ _)) = (ident, Closure (fromCPS l) env)
defToPair _ _ = error "internal error"

seedGlobals :: Env -> Store -> [CPS.CDef] -> GenSymState (Env, Store)
seedGlobals env store [] = return (env, store)
seedGlobals env store (CPS.CDef ident _ : rest) = do
  (env', store') <- updateEnvAndStore env store [ident] [Void]
  seedGlobals env' store' rest

startingEnvStore :: GenSymState (Env, Store)
startingEnvStore =
  uncurry (updateEnvAndStore Map.empty Map.empty)
  (unzip [("_halt", PrimHalt),
          ("+", PrimAdd),
          ("-", PrimSub),
          ("*", PrimMult),
          ("=", PrimEq),
          ("void", PrimVoid),
          ("call/cc", PrimCallCC),
          ("begin", PrimBegin)])

runMachine :: CPS.CProg -> GenSymState Value
runMachine (CPS.CProg defs expr) = do
  (env, store) <- startingEnvStore
  (env', store') <- seedGlobals env store defs
  let store'' = performSets env' store' (map (defToPair env') defs) in
    stepUntilHalt (CES (fromCPS expr) env' store'')
