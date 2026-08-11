module REPL where

import Lex
import Desugar
import Grammars
import InterpEnvDin

saca :: ASA -> String
saca (Num n) = show n
saca (Boolean b)
  | b == True = "#t"
  | otherwise = "#f"
saca (Fun _ _) = "#<procedure>"
saca expression = error ("Resultado inesperado: " ++ show expression)

-- Función encargada de llevar la ejecución del programa mediante los siguientes pasos:
-- 1. Impresión del prompt.
-- 2. Lectura de una cadena.
-- 3. Si la cadena es igual a "(exit)", se cierra el intérprete.
-- 4. En caso contrario, realiza la generación de código ejecutable aplicando los análisis en
--    orden siguiente: léxico, sintáctico, semántico.
-- 5. Vuelve a ejecutar el ciclo.
repl :: IO ()
repl =
  do
    putStr "> "
    str <- getLine
    if str == "(exit)"
      then putStrLn "Bye."
      else do
        putStrLn $ saca (interp (desugar (parse (lexer str))) [])
        repl

-- Función principal. Da la bienvenida al usuario y ejecuta el REPL.
run :: IO ()
run =
  do
    putStrLn "Mini-Lisp v3.2. Bienvenidx."
    repl
