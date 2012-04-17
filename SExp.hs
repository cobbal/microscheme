module SExp where
import Data.Char

type Identifier = String

data SExp = SId Identifier
          | SBool Bool
          | SNumber Integer
          | SList [SExp]

instance Show SExp where
  show (SNumber n) = show n
  show (SId i) = i
  show (SBool b) = show b
  show (SList l) = show l

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
  | isDigit c = TNumber (read text) : mLex rest
  | otherwise = TId text : mLex rest
  where (text, rest) = break tokenBoundary s
        tokenBoundary :: Char -> Bool
        tokenBoundary ch
          | isSpace ch = True
          | isRParen ch = True
          | isLParen ch = True
          | otherwise = False

parseSExps :: [Token] -> Maybe [SExp]
parseSExps tokens =
  case (parseAllThe parseSubExp) tokens of
    Just (exps, []) -> Just exps
    _ -> Nothing

type ExpParser = [Token] -> Maybe (SExp, [Token])
parseSubExp :: ExpParser
parseSubExp tokens = firstOneThatWorks [parseSId, parseSBool, parseSNumber, parseSList] tokens

parseSId :: ExpParser
parseSId (TId s : rest) = Just (SId s, rest)
parseSId _ = Nothing

parseSBool :: ExpParser
parseSBool (TBool b : rest) = Just (SBool b, rest)
parseSBool _ = Nothing

parseSNumber :: ExpParser
parseSNumber (TNumber n : rest) = Just (SNumber n, rest)
parseSNumber _ = Nothing

parseSList :: ExpParser
parseSList tokens = do
  (exps, tokens2) <- eat TLeftParen tokens >>=
                     parseAllThe parseSubExp
  tokens3 <- eat TRightParen tokens2
  return (SList exps, tokens3)

eat :: Token -> [Token] -> Maybe [Token]
eat expected (token : rest)
  | token == expected = Just rest
eat _ _ = Nothing

parseAllThe :: ([Token] -> Maybe (a, [Token])) -> [Token] -> Maybe ([a], [Token])
parseAllThe parseFn tokens = Just (parseAllHelp tokens)
  where parseAllHelp subtok =
          case parseFn subtok of
            Just (a, rest) -> let (as, rest2) = parseAllHelp rest in (a : as, rest2)
            Nothing -> ([], subtok)

firstOneThatWorks :: [(a -> Maybe b)] -> a -> Maybe b
firstOneThatWorks [] _ = Nothing
firstOneThatWorks (f : fs) a = case f a of
  Just x -> Just x
  Nothing -> firstOneThatWorks fs a
