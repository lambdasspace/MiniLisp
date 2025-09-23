{
module Grammars where

import Lex (Token(..),lexer)
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

%%

ASA : var                             { Id $1 }
    | int                             { Num $1 }
    | bool                            { Boolean $1 }
    | '(' '+' ASA ASA ')'             { Add $3 $4}
    | '(' '-' ASA ASA ')'             { Sub $3 $4}
    | '(' "not" ASA ')'               { Not $3 }
    | '(' let '(' var ASA ')' ASA ')' { Let $4 $5 $7 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Id String
          | Num Int
          | Boolean Bool
          | Add ASA ASA
          | Sub ASA ASA
          | Not ASA
          | Let String ASA ASA
          deriving(Show)
}
