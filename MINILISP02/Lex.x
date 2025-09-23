{
module Lex (Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"

-- Usamos códigos hex para los espacios en blanco Unicode más comunes:
--   \x20 = ' ' (space), \x09 = tab, \x0A = LF, \x0D = CR, \x0C = FF, \x0B = VT
$white = [\x20\x09\x0A\x0D\x0C\x0B]
$digit = 0-9
$letter = [A-Za-z_]
$idrest = [A-Za-z0-9_]

tokens :-

  -- Ignorar cualquier secuencia de espacios en blanco
  $white+               ;

  \(                    { \_ -> TokenPA }
  \)                    { \_ -> TokenPC }
  \+                    { \_ -> TokenSuma }
  \-                    { \_ -> TokenResta }
  not                   { \_ -> TokenNot }
  let                   { \_ -> TokenLet }

  "#t"                  { \_ -> TokenBool True }
  "#f"                  { \_ -> TokenBool False }

  $digit+               { \s -> TokenNum (read s) }

  $letter$idrest*       { \s -> TokenId s }

  -- Catch-all para diagnosticar caracteres inesperados
  .                     { \s -> error ("Lexical error: caracter no reconocido = "
                                      ++ show s
                                      ++ " | codepoints = "
                                      ++ show (map fromEnum s)) }

{
data Token
  = TokenId String
  | TokenNum Int
  | TokenBool Bool
  | TokenSuma
  | TokenResta
  | TokenNot
  | TokenPA
  | TokenPC
  | TokenLet
  deriving (Show)

-- Normaliza cualquier espacios en blanco Unicode a ' ' para que $white+ lo consuma
normalizeSpaces :: String -> String
normalizeSpaces = map (\c -> if isSpace c then '\x20' else c)

-- Alias: Alex define alexScanTokens (String -> [Token])
lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}
