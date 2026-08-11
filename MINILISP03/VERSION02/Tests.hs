module Main where

import Desugar
import InterpEnvDin

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = putStrLn ("OK: " ++ label)
  | otherwise = error
      (label ++ ": se esperaba " ++ show expected ++
       ", pero se obtuvo " ++ show actual)

scopeProgram :: ASA
scopeProgram =
  App
    (Fun "x"
      (App
        (Fun "f"
          (App
            (Fun "x" (App (Id "f") (Num 4)))
            (Num 5)))
        (Fun "y" (Add (Id "x") (Id "y")))))
    (Num 3)

restorationProgram :: ASA
restorationProgram =
  App
    (Fun "x"
      (Add
        (App (Fun "x" (Id "x")) (Num 1))
        (Id "x")))
    (Num 10)

main :: IO ()
main = do
  assertEqual "alcance dinámico" (Num 9) (interp scopeProgram [])
  assertEqual "restauración del ambiente" (Num 11)
    (interp restorationProgram [])
  assertEqual "resta truncada" (Num 0) (interp (Sub (Num 2) (Num 5)) [])
  assertEqual "negación numérica" (Boolean False)
    (interp (Not (Num 0)) [])
  assertEqual "los valores no dan pasos" Nothing
    (smallStep (Num 1) [])
  assertEqual "una suma mal formada queda bloqueada" Nothing
    (smallStep (Add (Boolean True) (Num 1)) [])
