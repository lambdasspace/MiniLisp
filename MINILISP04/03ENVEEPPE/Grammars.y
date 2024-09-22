{
module Grammars where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      var             { TokenId $$ }
      int             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      "not"           { TokenNot }
      '('             { TokenPA }
      ')'             { TokenPC }
      let             { TokenLet }
      lambda          { TokenLambda }
      if0             { TokenIf0 }

%%

SASA : var                               { IdS $1 }
     | int                               { NumS $1 }
     | bool                              { BooleanS $1 }
     | '(' '+' SASA SASA ')'             { AddS $3 $4}
     | '(' '-' SASA SASA ')'             { SubS $3 $4}
     | '(' "not" SASA ')'                { NotS $3 }
     | '(' let '(' var SASA ')' SASA ')' { LetS $4 $5 $7 }
     | '(' if0 SASA SASA SASA ')'        { If0S $3 $4 $5 }
     | '(' lambda '(' var ')' SASA ')'   { FunS $4 $6 }
     | '(' SASA SASA ')'                 { AppS $2 $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data SASA = IdS String
          | NumS Int
          | BooleanS Bool
          | AddS SASA SASA
          | SubS SASA SASA
          | NotS SASA
          | LetS String SASA SASA
          | If0S SASA SASA SASA
          | FunS String SASA
          | AppS SASA SASA 
          deriving(Show)

data Token = TokenId String
           | TokenNum Int
           | TokenBool Bool
           | TokenSuma
           | TokenResta
           | TokenNot
           | TokenPA
           | TokenPC
           | TokenLet
           | TokenIf0
           | TokenLambda
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
lexer ('l':'e':'t':xs) = TokenLet:(lexer xs)
lexer ('i':'f':'0':xs) = TokenIf0:(lexer xs)
lexer ('l':'a':'m':'b':'d':'a':xs) = TokenLambda:(lexer xs)
lexer (x:xs)
    | isDigit x = lexNum (x:xs)
    | isAlpha x = lexAlph (x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexAlph :: String -> [Token]
lexAlph cs = TokenId var : lexer rest
      where (var,rest) = span isAlpha cs

main = getContents >>= print . parse . lexer

}