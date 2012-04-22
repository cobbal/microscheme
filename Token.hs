module Token where

import Identifier (Identifier)
import Data.Char (isSpace, isDigit)

data Token = LeftParen Char
           | RightParen Char
           | Number Integer
           | Id Identifier
           | Boolean Bool

instance Show Token where
  show (LeftParen c) = "'" ++ [c] ++ "'"
  show (RightParen c) = "'" ++ [c] ++ "'"
  show (Number i) = "<num " ++ show i ++ ">"
  show (Id s) = "<ID " ++ show s ++ ">"
  show (Boolean False) = "<#f>"
  show (Boolean True) = "<#t>"

instance Eq Token where
  LeftParen c1 == LeftParen c2 = (c1 == c2)
  RightParen c1 == RightParen c2 = (c1 == c2)
  Number a == Number b = a == b
  Id a == Id b = a == b
  Boolean a == Boolean b = a == b
  _ == _ = False

isLParen :: Char -> Bool
isLParen c = c `elem` "([{"

isRParen :: Char -> Bool
isRParen c = c `elem` ")]}"

tokenize :: String -> [Token]

tokenize "" = []

tokenize (';' : s) = tokenize (dropWhile (/= '\n') s)
tokenize (c : s)
  | isSpace c = tokenize s
  | isLParen c = LeftParen c : tokenize s
  | isRParen c = RightParen c : tokenize s
tokenize ('#' : 't' : s) = Boolean True : tokenize s
tokenize ('#' : 'f' : s) = Boolean False : tokenize s

tokenize s@(c : _)
  | isDigit c = Number (read text) : tokenize rest
  | otherwise = Id text : tokenize rest
  where (text, rest) = break tokenBoundary s
        tokenBoundary :: Char -> Bool
        tokenBoundary ch
          | isSpace ch = True
          | isRParen ch = True
          | isLParen ch = True
          | otherwise = False

