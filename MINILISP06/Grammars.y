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
      letcc           { TokenLetCC }
      if0             { TokenIf0 }
      lambda          { TokenLambda }

%%

SASA : var                                 { IdS $1 }
     | int                                 { NumS $1 }
     | bool                                { BooleanS $1 }

     | '(' '+'   SASA SASA ')'             { AddS $3 $4 }
     | '(' '-'   SASA SASA ')'             { SubS $3 $4 }

     | '(' "not" SASA ')'                  { NotS $3 }

     | '(' let    '(' var SASA ')' SASA ')' { LetS    $4 $5 $7 }

     | '(' letcc var SASA ')'              { LetCCS $3 $4 } 

     | '(' if0 SASA SASA SASA ')'          { If0S $3 $4 $5 }

     | '(' lambda '(' var ')' SASA ')'     { FunS $4 $6 }

     | '(' SASA SASA ')'                   { AppS $2 $3 }

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
          | LetCCS String SASA
          | If0S SASA SASA SASA
          | FunS String SASA
          | AppS SASA SASA 
          deriving(Show)
}
