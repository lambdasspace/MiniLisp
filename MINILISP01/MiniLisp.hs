module REPL where

import Grammars
import Interp

saca :: ASA -> String
saca (Num n) = show n
saca (Boolean b)
    | b == True = "#t"
    | otherwise = "#f"

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
        if str == "(exit)" then 
            putStrLn "Bye." 
        else 
            do
                putStrLn $ saca (interp (parse (lexer str)))
                repl

-- Función principal. Da la bienvenida al usuario y ejecuta el REPL.
run =
    do
        putStrLn "Mini-Lisp v1.0. Bienvenidx." 
        repl