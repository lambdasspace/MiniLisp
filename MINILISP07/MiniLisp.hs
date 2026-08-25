module MiniLisp where

import Desugar
import Grammars
import Interp
import Lex

saca :: ASAValues -> String
saca (NumV number) = show number
saca (BooleanV True) = "#t"
saca (BooleanV False) = "#f"
saca (ClosureV _ _ _) = "#<procedure>"
saca (ContV _) = "#<continuation>"
saca value = show value

prelude :: Env
prelude = []

compile :: String -> ASAValues
compile source =
  desugarV (desugar (parse (lexer source)))

runSource :: String -> Either String ASAValues
runSource source =
  execute (compile source) prelude

trace :: String -> [State]
trace source =
  traceMachine (injectWithEnv (compile source) prelude)

printTrace :: String -> IO ()
printTrace source =
  mapM_ print (trace source)

repl :: IO ()
repl = do
  putStr "> "
  source <- getLine
  if source == "(exit)"
    then putStrLn "Bye."
    else do
      case runSource source of
        Right value -> putStrLn (saca value)
        Left message -> putStrLn ("Error: " ++ message)
      repl

run :: IO ()
run = do
  putStrLn "Mini-Lisp v7 (maquina CEK con continuaciones). Bienvenidx."
  repl

test :: String -> IO ()
test source =
  case runSource source of
    Right value -> putStrLn (saca value)
    Left message -> putStrLn ("Error: " ++ message)

testLetCC :: IO ()
testLetCC =
  test "(+ 1 (+ (let/cc k (k 3)) 3))"

testEscape :: IO ()
testEscape =
  test
    "(let/cc abort ((lambda (n) (+ 100 (if0 n (abort 7) n))) 0))"

testSuma :: IO ()
testSuma =
  test
    "(letrec (sumN (lambda (n) (if0 n 0 (+ n (sumN (- n 1)))))) (sumN 3))"
