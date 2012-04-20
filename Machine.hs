module Machine where

import Data.List (intercalate)
import SExp
import CPS
import UnsafeLog

import qualified Data.Map as Map
import Data.Map ((!))

data Value = VClosure AExp Env
           | VBool Bool
           | VNum Integer
           | VUndefined
           | VPrimHalt
           | VPrimAdd
           | VPrimSub
           | VPrimMult
           | VPrimEq
           | VPrimVoid
           | VPrimCallCC
           | VPrimBegin

instance Show Value where
  show (VClosure _ _) = "<closure>"
  --show (VClosure a b) = show a ++ show b
  show (VBool b) = show b
  show (VNum i) = show i
  show VUndefined = "UNDEFINED"
  show VPrimHalt = "_halt"
  show VPrimAdd = "_+"
  show VPrimSub = "_-"
  show VPrimMult = "_*"
  show VPrimEq = "_="
  show VPrimVoid = "_void"
  show VPrimCallCC = "_call/cc"
  show VPrimBegin = "_begin"

instance Eq Value where
  (VBool a) == (VBool b) = (a == b)
  (VNum a) == (VNum b) = (a == b)
  _ == _ = False

type Env = Map.Map Identifier StoreAddress

type StoreAddress = Identifier
type Store = Map.Map StoreAddress Value

data Machine = CES CExp Env Store
             | Halt Value

instance Show Machine where
  show (CES expr env store) = "{" ++ show expr ++ "\n " ++
                              (intercalate ", " $ map foo (Map.keys env)) ++ "}"
    where foo :: Identifier -> String
          foo key = show key ++ " -> " ++ show (store ! (env ! key))
  show (Halt val) = "{" ++ show val ++ "}"

atomLookup :: Env -> Store -> AExp -> Value
atomLookup env store (MId ident)
  | ident `Map.member` env = store ! (env ! ident)
  | otherwise = error ("symbol " ++ show ident ++ " not found")
atomLookup _ _ (MNum n) = VNum n
atomLookup _ _ (MBool b) = VBool b
atomLookup _ _ (MVoid) = VUndefined
atomLookup env _ l@(MLambda _ _) = VClosure l env

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

evalPrim :: Value -> [Value] -> AExp -> CExp
evalPrim VPrimAdd [VNum a, VNum b] k = MApp k [MNum (a + b)]
evalPrim VPrimSub [VNum a, VNum b] k = MApp k [MNum (a - b)]
evalPrim VPrimMult [VNum a, VNum b] k = MApp k [MNum (a * b)]
evalPrim VPrimEq [a, b] k = MApp k [MBool (a == b)]
evalPrim VPrimVoid _ k = MApp k [MVoid]
evalPrim VPrimCallCC [v] k = MApp v [k, k]
evalPrim VPrimBegin [] k = MApp k [MVoid]
evalPrim VPrimBegin args k = MApp k [last args]

step :: Machine -> GenSymState Machine
step (Halt v) = return (Halt v)
step (CES (Atomic val) env store) = return (Halt (atomLookup env store val))
step (CES (MApp fn args) env store) =
  case atomLookup env store fn of
    VPrimHalt -> case args' of
      [v] -> return (Halt v)
      _ -> error "wrong number of args to VPrimHalt"

    VPrimAdd -> case (args, args') of
      ([_, _, k], [VNum a, VNum b, _]) -> return (CES (MApp k [MNum (a + b)]) env store)
      _ -> error "wrong number/type of args passer to VPrimAdd"
    VPrimSub -> case (args, args') of
      ([_, _, k], [VNum a, VNum b, _]) -> return (CES (MApp k [MNum (a - b)]) env store)
      _ -> error "wrong number/type of args passer to VPrimSub"
    VPrimMult -> case (args, args') of
      ([_, _, k], [VNum a, VNum b, _]) -> return (CES (MApp k [MNum (a * b)]) env store)
      _ -> error "wrong number/type of args passer to VPrimMult"
    VPrimEq -> case (args, args') of
      ([_, _, k], [a, b, _]) -> return (CES (MApp k [MBool (a == b)]) env store)
      _ -> error "wrong number/type of args passer to VPrimEq"
    VPrimVoid -> return (CES (MApp (last args) [MVoid]) env store)
    VPrimCallCC -> case (args, args') of
      ([_, k], [VClosure (MLambda [formal, cont] body) clEnv, kv]) -> do
        v <- gensym "v"
        unusedCont <- gensym "unusedCont"
        (env', store') <- updateEnvAndStore clEnv store
                          [formal, cont]
                          [VClosure (MLambda [v, unusedCont] (MApp k [MId v])) env, kv]
        return (CES body env' store')
      _ -> error $ "Error: wrong type arg to call/cc (" ++ show args' ++ ")"
    VPrimBegin -> case (reverse args, reverse args') of
      ([k], _) -> return (CES (MApp k [MVoid]) env store)
      ((k : _), (_ : importantVal : unimportantVals)) ->
        return (CES (MApp k [importantVal]) env store)

    VClosure (MLambda formals body) clEnv -> do
        (env', store') <- updateEnvAndStore clEnv store formals args'
        return (CES body env' store')
    result -> error $ "Error: application of non-(closure/primitive) " ++ show result
  where args' = map (atomLookup env store) args
step (CES (MIf condition consequent alternate) env store) = do
  case atomLookup env store condition of
    VBool False -> return (CES alternate env store)
    _ -> return (CES consequent env store)
step (CES (MSet ident value k) env store) =
  let store' = performSets env store [(ident, atomLookup env store value)] in do
    return (CES (MApp k [MVoid]) env store')

stepUntilHalt :: Machine -> GenSymState Value
stepUntilHalt (Halt v) = return v
-- stepUntilHalt m = step m >>= stepUntilHalt
stepUntilHalt m = step (unsafeLog m) >>= stepUntilHalt


defToPair :: Env -> CDef -> (Identifier, Value)
defToPair env (CDef ident l@(MLambda _ _)) = (ident, VClosure l env)
defToPair _ _ = error "internal error"

seedGlobals :: Env -> Store -> [CDef] -> GenSymState (Env, Store)
seedGlobals env store [] = return (env, store)
seedGlobals env store (CDef ident _ : rest) = do
  (env', store') <- updateEnvAndStore env store [ident] [VUndefined]
  seedGlobals env' store' rest

startingEnvStore :: GenSymState (Env, Store)
startingEnvStore =
  uncurry (updateEnvAndStore Map.empty Map.empty)
  (unzip [("_halt", VPrimHalt),
          ("+", VPrimAdd),
          ("-", VPrimSub),
          ("*", VPrimMult),
          ("=", VPrimEq),
          ("void", VPrimVoid),
          ("call/cc", VPrimCallCC),
          ("begin", VPrimBegin)])

runMachine :: CProg -> Value
runMachine (CProg defs expr) =
  runGenSymState (do
                     (env, store) <- startingEnvStore
                     (env', store') <- seedGlobals env store defs
                     let store'' = performSets env' store' (map (defToPair env') defs) in
                       stepUntilHalt (CES expr env' store''))
