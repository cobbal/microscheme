module MExpr where

import System.IO.Unsafe
import Data.Char

type Identifier = String

type MProg = ([MDef], MExpr)

type MDef = (Identifier, [Identifier], MExpr)

data MExpr = MNumber Integer
           | MVar Identifier
           | MBool Bool
           | MLambda [Identifier] MExpr
           | MLet [(Identifier, MExpr)] MExpr
           | MIf MExpr MExpr MExpr
           | MSet Identifier MExpr
           | MBegin [MExpr]
           | MPrimApplication Identifier [MExpr]
           | MApplication MExpr [MExpr]
           | MCallCC MExpr

instance Show MExpr where
  show (MNumber i) = "{num " ++ show i ++ "}"
  show (MVar v) = "{var " ++ show v ++ "}"
  show (MBool b) = "{bool " ++ show b ++ "}"
  show (MLambda ids exp) = "{lambda " ++ show ids ++ " " ++ show exp ++ "}"
  show (MLet _ _) = "{LET}"
  show (MIf _ _ _) = "{IF}"
  show (MSet _ _) = "{SET}"
  show (MBegin _) = "{BEGIN}"
  show (MPrimApplication _ _) = "{PRIM APPLICATION}"
  show (MApplication _ _) = "{APPLICATION}"
  show (MCallCC _) = "{CALL/CC}"

data Token = TLeftParen
           | TRightParen
           | TNumber Integer
           | TId Identifier
           | TBool Bool

instance Show Token where
  show TLeftParen = "'('"
  show TRightParen = "')'"
  show (TNumber i) = "<num " ++ show i ++ ">"
  show (TId s) = "<ID " ++ show s ++ ">"
  show (TBool False) = "<#f>"
  show (TBool True) = "<#t>"

instance Eq Token where
  TLeftParen == TLeftParen = True
  TRightParen == TRightParen = True
  TNumber a == TNumber b = a == b
  TId a == TId b = a == b
  TBool a == TBool b = a == b
  _ == _ = False

unsafeLog :: Show a => a -> a
unsafeLog x = unsafePerformIO $ print x >> return x

isLParen :: Char -> Bool
isLParen c = c `elem` "([{"

isRParen :: Char -> Bool
isRParen c = c `elem` ")]}"

mLex :: String -> [Token]

mLex "" = []

mLex (whitespace : s)
  | isSpace whitespace = mLex s

mLex (c : s)
  | c `elem` "([{" = TLeftParen : mLex s
mLex ('[' : s) = TLeftParen : mLex s
mLex ('{' : s) = TLeftParen : mLex s

mLex (')' : s) = TRightParen : mLex s
mLex (']' : s) = TRightParen : mLex s
mLex ('}' : s) = TRightParen : mLex s

mLex ('#' : 't' : s) = TBool True : mLex s
mLex ('#' : 'f' : s) = TBool False : mLex s

mLex s@(c : _)
  | isDigit c = TNumber (read text) : mLex remainder
  | otherwise = TId text : mLex remainder
  where (text, remainder) = break tokenBoundary s
        tokenBoundary :: Char -> Bool
        tokenBoundary c
          | isSpace c = True
          | isRParen c = True
          | isLParen c = True
          | otherwise = False

-- type MProg = ([MDef], MExpr)
parseProg :: [Token] -> Maybe MProg
parseProg tokens = do
  (defs, tokens) <- parseAllThe parseDef tokens
  (exp, tokens) <- parseExp tokens
  if tokens == []
    then return (defs, exp)
    else Nothing

parseAllThe :: ([Token] -> Maybe (a, [Token])) -> [Token] -> Maybe ([a], [Token])
parseAllThe parseFn tokens = Just (parseAllHelp parseFn tokens)
  where parseAllHelp :: ([Token] -> Maybe (a, [Token])) -> [Token] -> ([a], [Token])
        parseAllHelp parseFn tokens =
          case parseFn tokens of
            Just (a, rest) -> let (as, rest2) = parseAllHelp parseFn rest in (a : as, rest2)
            Nothing -> ([], tokens)

parseDef :: [Token] -> Maybe (MDef, [Token])
parseDef tokens = do
  (name, tokens) <- eat TLeftParen tokens >>=
                    eat (TId "define") >>=
                    eat TLeftParen >>=
                    parseId
  (formals, tokens) <- parseAllThe parseId tokens
  (exp, tokens) <- eat TRightParen tokens >>=
                   parseExp
  tokens <- eat TRightParen tokens
  return ((name, formals, exp), tokens)

parseId :: [Token] -> Maybe (Identifier, [Token])
parseId (TId s : remainder) = Just (s, remainder)
parseId _ = Nothing

firstOneThatWorks :: [(a -> Maybe b)] -> a -> Maybe b
firstOneThatWorks [] _ = Nothing
firstOneThatWorks (f : fs) a = case f a of
  Just x -> Just x
  Nothing -> firstOneThatWorks fs a

type ExpParser = [Token] -> Maybe (MExpr, [Token])

parseExp :: ExpParser
parseExp (TNumber i : rest) = Just (MNumber i, rest)
parseExp (TId v : rest) = Just (MVar v, rest)
parseExp (TBool b : rest) = Just (MBool b, rest)
parseExp tokens = firstOneThatWorks [parseLambda, parseLet, parseIf, parseSet,
                                     parseBegin, parseApp, parsePrimApp, parseCallCC] tokens

parseLambda :: ExpParser
parseLambda tokens = do
  (formals, tokens) <- eat TLeftParen tokens >>=
                       eat (TId "lambda") >>=
                       eat TLeftParen >>=
                       parseAllThe parseId
  (exp, tokens) <- eat TRightParen tokens >>=
                   parseExp
  tokens <- eat TRightParen tokens
  return (MLambda formals exp, tokens)

parseBinding :: [Token] -> Maybe ((Identifier, MExpr), [Token])
parseBinding tokens = do
  (var, tokens) <- eat TLeftParen tokens >>=
                   parseId
  (exp, tokens) <- parseExp tokens
  tokens <- eat TRightParen tokens
  return ((var, exp), tokens)

parseLet :: ExpParser
parseLet tokens = do
  (bindings, tokens) <- eat TLeftParen tokens >>=
                        eat (TId "let") >>=
                        eat TLeftParen >>=
                        parseAllThe parseBinding
  (exp, tokens) <- eat TRightParen tokens
                   >>= parseExp
  tokens <- eat TRightParen tokens
  return (MLet bindings exp, tokens)

parseIf :: ExpParser
parseIf tokens = do
  (condition, tokens) <- eat TLeftParen tokens >>=
                         eat (TId "if") >>=
                         parseExp
  (consequent, tokens) <- parseExp tokens
  (alternate, tokens) <- parseExp tokens
  tokens <- eat TRightParen tokens
  return (MIf condition consequent alternate, tokens)

parseSet :: ExpParser
parseSet tokens = do
  (var, tokens) <- eat TLeftParen tokens >>=
                   eat (TId "set!") >>=
                   parseId
  (exp, tokens) <- parseExp tokens
  tokens <- eat TRightParen tokens
  return (MSet var exp, tokens)

parseBegin :: ExpParser
parseBegin tokens = do
  (exps, tokens) <- eat TLeftParen tokens >>=
                    eat (TId "begin") >>=
                    parseAllThe parseExp
  tokens <- eat TRightParen tokens
  return (MBegin exps, tokens)

parseApp :: ExpParser
parseApp tokens = do
  (fn, tokens) <- eat TLeftParen tokens >>=
                  parseExp
  (args, tokens) <- parseAllThe parseExp tokens
  tokens <- eat TRightParen tokens
  return (MApplication fn args, tokens)

parsePrim :: [Token] -> Maybe (Identifier, [Token])
parsePrim (TId x : rest)
  | x `elem` ["+", "-", "*", "=", "void"] = Just (x, rest)
parsePrim _ = Nothing

parsePrimApp :: ExpParser
parsePrimApp tokens = do
  (prim, tokens) <- eat TLeftParen tokens >>=
                    parsePrim
  (args, tokens) <- parseAllThe parseExp tokens
  tokens <- eat TRightParen tokens
  return (MPrimApplication prim args, tokens)

parseCallCC :: ExpParser
parseCallCC tokens = do
  (exp, tokens) <- eat TLeftParen tokens >>=
                   eat (TId "call/cc") >>=
                   parseExp
  tokens <- eat TRightParen tokens
  return (MCallCC exp, tokens)

{-
data MExpr = MNumber Integer
           | MVar Identifier
           | MBool Bool
           | MLambda [Identifier] MExpr
           | MLet [(Identifier, MExpr)] MExpr
           | MIf MExpr MExpr MExpr
           | MSet Identifier MExpr
           | MBegin [MExpr]
           | MPrimApplication Identifier [MExpr]
           | MApplication MExpr [MExpr]
           | MCallCC MExpr
-}

eat :: Token -> [Token] -> Maybe [Token]
eat expected (token : rest)
  | token == expected = Just rest
eat _ _ = Nothing
