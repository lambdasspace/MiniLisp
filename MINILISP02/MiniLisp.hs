module REPL where

import Lex
import Grammars
import Interp

-- Convierte un ASA en su representación textual
saca :: ASA -> String
saca (Num n)     = show n
saca (Boolean b) = if b then "#t" else "#f"

-- Ciclo interactivo (REPL)
repl :: IO ()
repl = do
  putStr "> "
  str <- getLine
  if str == "(exit)"
    then putStrLn "Bye."
    else do
      putStrLn $ saca (interp (parse (lexer str)))
      repl

-- Inicia el intérprete
run :: IO ()
run = do
  putStrLn "Mini-Lisp v2.0. Bienvenidx."
  repl
