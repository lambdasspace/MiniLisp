module Main where

import Desugar
import Interp
import MiniLisp (runSource, trace)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = putStrLn ("OK: " ++ label)
  | otherwise =
      error
        ( label
            ++ ": se esperaba "
            ++ show expected
            ++ ", pero se obtuvo "
            ++ show actual
        )

assertFinal :: String -> ASAValues -> [State] -> IO ()
assertFinal label expected states =
  case reverse states of
    Return Mt actual : _ -> assertEqual label expected actual
    finalState : _ -> error (label ++ ": estado final inesperado " ++ show finalState)
    [] -> error (label ++ ": la traza esta vacia")

main :: IO ()
main = do
  assertEqual
    "programa conductor"
    (Right (NumV 15))
    (runSource "(let (x 10) ((lambda (y) (+ x y)) 5))")

  assertEqual
    "alcance estatico"
    (Right (NumV 15))
    ( runSource
        "(let (x 10) (let (f (lambda (y) (+ x y))) (let (x 20) (f 5))))"
    )

  assertEqual
    "ambiente recursivo"
    (Right (NumV 6))
    ( runSource
        "(letrec (sumN (lambda (n) (if0 n 0 (+ n (sumN (- n 1)))))) (sumN 3))"
    )

  assertEqual
    "gramatica completa de MiniLisp05"
    (Right (NumV 120))
    ( runSource
        "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))"
    )

  assertEqual
    "la rama descartada no se evalua"
    (Right (NumV 1))
    (runSource "(if #t 1 libre)")

  assertEqual
    "negacion booleana"
    (Right (BooleanV True))
    (runSource "(not #f)")

  assertEqual
    "resta entera"
    (Right (NumV (-3)))
    (runSource "(- 2 5)")

  assertEqual
    "bloqueo aritmetico"
    (Left "La suma espera numeros")
    (runSource "(+ #t 1)")

  assertEqual
    "bloqueo de aplicacion"
    (Left "La aplicacion espera una funcion")
    (runSource "(1 2)")

  assertEqual
    "variable libre"
    (Left "Variable libre: libre")
    (runSource "libre")

  let conductorTrace = trace "(let (x 10) ((lambda (y) (+ x y)) 5))"
  assertFinal "resultado de la traza" (NumV 15) conductorTrace

  let divergentPrefix =
        take 25
          ( trace
              "(letrec (loop (lambda (x) (loop x))) (loop 0))"
          )
  assertEqual "prefijo de una ejecucion divergente" 25 (length divergentPrefix)
