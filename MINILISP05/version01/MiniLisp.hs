module REPL where

import Lex
import Desugar
import Grammars
import Interp

saca :: ASAValues -> String
saca (NumV n) = show n
saca (BooleanV True) = "#t"
saca (BooleanV False) = "#f"
saca (ClosureV _ _ _) = "#<procedure>"
saca _ = "#<valor-desconocido>"

prelude :: Env
prelude = []

repl :: IO ()
repl = do
  putStr "> "
  str <- getLine
  if str == "(exit)"
    then putStrLn "Bye."
    else do
      putStrLn $ saca (interp (desugarV (desugar (parse (lexer str)))) prelude)
      repl

run :: IO ()
run = do
  putStrLn "Mini-Lisp v5.1 (ambientes recursivos). Bienvenidx."
  repl

test :: String -> IO ()
test source =
  putStrLn $ saca (interp (desugarV (desugar (parse (lexer source)))) prelude)

testSuma = test "(letrec (sumN (lambda (n) (if0 n 0 (+ n (sumN (- n 1)))))) (sumN 3))"
testFactorial = test "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))"
testFibo = test "(letrec (fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 5))"
