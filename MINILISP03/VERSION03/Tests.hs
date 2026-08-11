module Main where

import Desugar
import InterpEnvEst

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = putStrLn ("OK: " ++ label)
  | otherwise = error
      (label ++ ": se esperaba " ++ show expected ++
       ", pero se obtuvo " ++ show actual)

scopeProgram :: ASAValues
scopeProgram = desugarV
  (App
    (Fun "x"
      (App
        (Fun "f"
          (App
            (Fun "x" (App (Id "f") (Num 4)))
            (Num 5)))
        (Fun "y" (Add (Id "x") (Id "y")))))
    (Num 3))

restorationProgram :: ASAValues
restorationProgram = desugarV
  (App
    (Fun "x"
      (Add
        (App (Fun "x" (Id "x")) (Num 1))
        (Id "x")))
    (Num 10))

escapingClosureProgram :: ASAValues
escapingClosureProgram = desugarV
  (App
    (Fun "make"
      (App
        (Fun "add3" (App (Id "add3") (Num 4)))
        (App (Id "make") (Num 3))))
    (Fun "x" (Fun "y" (Add (Id "x") (Id "y")))))

main :: IO ()
main = do
  assertEqual "alcance estático" (NumV 7) (interp scopeProgram [])
  assertEqual "restauración del ambiente" (NumV 11)
    (interp restorationProgram [])
  assertEqual "cerradura que escapa" (NumV 7)
    (interp escapingClosureProgram [])
  assertEqual "resta truncada" (NumV 0)
    (interp (desugarV (Sub (Num 2) (Num 5))) [])
  assertEqual "negación numérica" (BooleanV False)
    (interp (desugarV (Not (Num 0))) [])
  assertEqual "un literal produce su valor de ejecución"
    (Just (NumV 1, [])) (smallStep (NumC 1) [])
  assertEqual "los valores no dan pasos" Nothing
    (smallStep (NumV 1) [])
  assertEqual "una suma mal formada queda bloqueada" Nothing
    (smallStep (AddV (BooleanV True) (NumV 1)) [])
