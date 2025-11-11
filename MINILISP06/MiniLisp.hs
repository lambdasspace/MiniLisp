module MiniLisp where

import Lex
import Desugar
import Grammars
import Interp

combinadorY :: String
combinadorY = "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))"

saca :: ASAValues -> String
saca (NumV n) = show n
saca (BooleanV b)
  | b == True = "#t"
  | otherwise = "#f"
saca (ClosureV _ _ _) = "#<procedure>"
saca (ContV _ _) = "#<continuation>"

-- Función encargada de llevar la ejecución del programa mediante los siguientes pasos:
-- 1. Impresión del propt.
-- 2. Lectura de una cadena.
-- 3. Si la cadana es igual a ":q", se cierra el intérprete.
-- 4. En caso contrario, realiza la generación de código ejecutable aplicando los análisis en
--    orden siguiente: léxico, sintáctico, semántico.
-- 5. Vuelve a ejecutar el ciclo.
repl =
  do
    putStr "> "
    str <- getLine
    if str == "(exit)"
      then putStrLn "Bye."
      else do
        putStrLn $ saca (interp (desugarV (desugar (parse (lexer str)))) [] HaltV)
        repl

-- Función principal. Da la bienvenida al usuario y ejecuta el REPL.
run =
  do
    putStrLn "Mini-Lisp v6.0. Bienvenidx."
    repl

test :: String -> IO ()
test x = putStrLn $ saca (interp (desugarV (desugar (parse (lexer x)))) [] HaltV)

-- Pruebas (asumiendo que letrec se desazucara vía Z)
testLetCC  = test "(+ 1 (+ (let/cc k (k 3)) 3))"      -- 7