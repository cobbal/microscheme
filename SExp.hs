module SExp where

import qualified Token
import Token (Token)
import Identifier (Identifier)

data SExp = Id Identifier
          | Boolean Bool
          | Number Integer
          | List [SExp]

instance Show SExp where
  show (Number n) = show n
  show (Id i) = i
  show (Boolean b) = show b
  show (List l) = show l

closeParenFor :: Char -> Char
closeParenFor '(' = ')'
closeParenFor '[' = ']'
closeParenFor '{' = '}'
closeParenFor c = error ("Don't know how to close paren `" ++ [c] ++ "'.")

parseSExps :: [Token] -> Maybe [SExp]
parseSExps tokens =
  case (parseAllThe parseSubExp) tokens of
    Just (exps, []) -> Just exps
    _ -> Nothing

type ExpParser = [Token] -> Maybe (SExp, [Token])
parseSubExp :: ExpParser
parseSubExp tokens = firstOneThatWorks [parseId, parseBoolean, parseNumber, parseList] tokens

parseId :: ExpParser
parseId (Token.Id s : rest) = Just (Id s, rest)
parseId _ = Nothing

parseBoolean :: ExpParser
parseBoolean (Token.Boolean b : rest) = Just (Boolean b, rest)
parseBoolean _ = Nothing

parseNumber :: ExpParser
parseNumber (Token.Number n : rest) = Just (Number n, rest)
parseNumber _ = Nothing

parseList :: ExpParser
parseList (Token.LeftParen c : tokens) = do
  (exps, tokens') <- parseAllThe parseSubExp tokens
  tokens'' <- eat (Token.RightParen (closeParenFor c)) tokens'
  return (List exps, tokens'')
parseList _ = Nothing

eat :: Token -> [Token] -> Maybe [Token]
eat expected (token : rest)
  | token == expected = Just rest
eat _ _ = Nothing

parseAllThe :: ([Token] -> Maybe (a, [Token])) -> [Token] -> Maybe ([a], [Token])
parseAllThe parseFn tokens = Just (parseAllHelp tokens)
  where parseAllHelp subtok =
          case parseFn subtok of
            Just (a, rest) -> let (as, rest') = parseAllHelp rest in (a : as, rest')
            Nothing -> ([], subtok)

firstOneThatWorks :: [(a -> Maybe b)] -> a -> Maybe b
firstOneThatWorks [] _ = Nothing
firstOneThatWorks (f : fs) a = case f a of
  Just x -> Just x
  Nothing -> firstOneThatWorks fs a
