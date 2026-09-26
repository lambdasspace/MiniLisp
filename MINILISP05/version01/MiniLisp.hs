module REPL where

import Lex
import Desugar
import Grammars
import Interp

combinadorY :: String
combinadorY =
  "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))"

y :: ASAValues
y =
  let sasa = parse (lexer combinadorY)
      asa = desugar sasa
  in interp (desugarV asa) []

prelude :: Env
prelude = [("Y", y)]

saca :: ASAValues -> String
saca (NumV number) = show number
saca (BooleanV True) = "#t"
saca (BooleanV False) = "#f"
saca (ExprV _ _) = "#<expresion>"
saca (ClosureV _ _ _) = "#<procedure>"
saca _ = "#<valor-desconocido>"

runProgram :: ASA -> ASAValues
runProgram expression =
  strict (interp (desugarV expression) prelude)

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
  putStrLn "Mini-Lisp v5.1 (Y y evaluación perezosa). Bienvenidx."
  repl

test :: String -> IO ()
test source =
  putStrLn $ saca (runProgram (desugar (parse (lexer source))))

testSuma =
  test "(letrec (sum (lambda (n) (if0 n 0 (+ n (sum (- n 1)))))) (sum 3))"
