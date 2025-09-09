{
module Grammars where

import Lex (Token(..), lexer)
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

ASA : int                      { Num $1 }
    | bool                     { Boolean $1 }
    | '(' '+' ASA ASA ')'      { Add $3 $4 }
    | '(' '-' ASA ASA ')'      { Sub $3 $4 }
    | '(' "not" ASA ')'        { Not $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data ASA
  = Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  deriving (Show)
}
