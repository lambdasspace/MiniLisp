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

isContinuationApplication :: State -> Bool
isContinuationApplication (Return (ArgK (ContV _) _) _) = True
isContinuationApplication _ = False

main :: IO ()
main = do
  assertEqual
    "continuacion capturada pero no aplicada"
    (Right (NumV 5))
    (runSource "(let/cc k 5)")

  assertEqual
    "la continuacion descarta el contexto actual"
    (Right (NumV 4))
    (runSource "(+ 1 (let/cc k (+ 100 (k 3))))")

  assertEqual
    "la continuacion reinstala el contexto capturado"
    (Right (NumV 7))
    (runSource "(+ 1 (+ (let/cc k (k 3)) 3))")

  assertEqual
    "escape temprano de una aplicacion"
    (Right (NumV 7))
    ( runSource
        "(let/cc abort ((lambda (n) (+ 100 (if0 n (abort 7) n))) 0))"
    )

  assertEqual
    "una funcion conserva el contexto actual"
    (Right (NumV 104))
    (runSource "(+ 1 ((lambda (x) (+ 100 x)) 3))")

  assertEqual
    "programa conductor heredado de MiniLisp06"
    (Right (NumV 15))
    (runSource "(let (x 10) ((lambda (y) (+ x y)) 5))")

  assertEqual
    "alcance estatico heredado de MiniLisp06"
    (Right (NumV 15))
    ( runSource
        "(let (x 10) (let (f (lambda (y) (+ x y))) (let (x 20) (f 5))))"
    )

  assertEqual
    "ambiente recursivo heredado de MiniLisp06"
    (Right (NumV 6))
    ( runSource
        "(letrec (sumN (lambda (n) (if0 n 0 (+ n (sumN (- n 1)))))) (sumN 3))"
    )

  assertEqual
    "gramatica completa heredada de MiniLisp06"
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
    (Left "La aplicacion espera una funcion o una continuacion")
    (runSource "(1 2)")

  assertEqual
    "variable libre"
    (Left "Variable libre: libre")
    (runSource "libre")

  let continuationTrace =
        trace "(+ 1 (let/cc k (+ 100 (k 3))))"
  assertFinal "resultado de la traza" (NumV 4) continuationTrace
  assertEqual
    "la traza muestra la aplicacion de ContV"
    True
    (any isContinuationApplication continuationTrace)

  let divergentPrefix =
        take 25
          ( trace
              "(letrec (loop (lambda (x) (loop x))) (loop 0))"
          )
  assertEqual "prefijo de una ejecucion divergente" 25 (length divergentPrefix)
