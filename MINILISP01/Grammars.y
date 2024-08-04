{
module Grammars where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      int             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      "not"           { TokenNot }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : int                  { Num $1 }
    | bool                 { Boolean $1 }
    | '(' '+' ASA ASA ')'  { Add $3 $4}
    | '(' '-' ASA ASA ')'  { Sub $3 $4}
    | '(' "not" ASA ')'    { Not $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Num Int
          | Boolean Bool
          | Add ASA ASA
          | Sub ASA ASA
          | Not ASA
          deriving(Show)

data Token = TokenNum Int
           | TokenBool Bool
           | TokenSuma
           | TokenResta
           | TokenNot
           | TokenPA
           | TokenPC
           deriving(Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ' : xs) = lexer xs
lexer ('(' : xs) = TokenPA:(lexer xs)
lexer (')' : xs) = TokenPC:(lexer xs)
lexer ('+' : xs) = TokenSuma:(lexer xs)
lexer ('-' : xs) = TokenResta:(lexer xs)
lexer ('n':'o':'t':xs) = TokenNot:(lexer xs)
lexer ('#':'t':xs) = (TokenBool True):(lexer xs)
lexer ('#':'f':xs) = (TokenBool False):(lexer xs)
lexer (x:xs)
    | isDigit x = lexNum (x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

main = getContents >>= print . parse . lexer

}