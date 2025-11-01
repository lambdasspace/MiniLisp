module REPL where

import Lex
import Desugar
import Grammars
import Interp

combinadorY :: String
combinadorY =
  "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))"

-- Valor de Y ya desazucarado y listo para usar en el ambiente inicial
y :: ASAValues
y =
  let sasa = parse (lexer combinadorY)  -- SASA
      asa  = desugar sasa               -- ASA
  in interp (desugarV asa) []           -- ASAValues evaluado en []

saca :: ASAValues -> String
saca (NumV n) = show n
saca (BooleanV b)
  | b == True  = "#t"
  | otherwise  = "#f"
saca (ExprV _ _)      = "#<expresion>"
saca (ClosureV _ _ _) = "#<procedure>"
saca _                = "#<valor-desconocido>"

prelude :: Env
prelude = [("Y", y)]

repl = do
  putStr "> "
  str <- getLine
  if str == "(exit)"
    then putStrLn "Bye."
    else do
      putStrLn $ saca (interp (desugarV (desugar (parse (lexer str)))) prelude)
      repl

run = do
  putStrLn "Mini-Lisp v5.1. Bienvenidx."
  repl

test x = putStrLn $ saca (interp (desugarV (desugar (parse (lexer x)))) prelude)
testSuma = test "(letrec (sumN (lambda (n) (if0 n 0 (+ n (sumN (- n 1)))))) (sumN 3))" -- 6
testFactorial = test "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))" -- 120
testFibo = test "(letrec (fib (lambda (n) (if (<= n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 5))" -- 5
