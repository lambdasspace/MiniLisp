module REPL where

import Lex
import Desugar
import Grammars
import Interp

combinadorZ :: String
combinadorZ =
  "(lambda (f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v))))))"

z :: ASAValues
z =
  let sasa = parse (lexer combinadorZ)
      asa = desugar sasa
  in interp (desugarV asa) []

prelude :: Env
prelude = [("Z", z)]

saca :: ASAValues -> String
saca (NumV number) = show number
saca (BooleanV True) = "#t"
saca (BooleanV False) = "#f"
saca (ClosureV _ _ _) = "#<procedure>"
saca _ = "#<valor-desconocido>"

runProgram :: ASA -> ASAValues
runProgram expression =
  interp (desugarV expression) prelude

repl :: IO ()
repl = do
  putStr "> "
  source <- getLine
  if source == "(exit)"
    then putStrLn "Bye."
    else do
      putStrLn $ saca (runProgram (desugar (parse (lexer source))))
      repl

run :: IO ()
run = do
  putStrLn "Mini-Lisp v5.2 (Z y evaluación ansiosa). Bienvenidx."
  repl

test :: String -> IO ()
test source =
  putStrLn $ saca (runProgram (desugar (parse (lexer source))))

testSuma =
  test "(letrec (sum (lambda (n) (if0 n 0 (+ n (sum (- n 1)))))) (sum 3))"
