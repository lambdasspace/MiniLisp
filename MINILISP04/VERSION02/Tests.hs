module Main where

import Desugar
import Interp

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = putStrLn ("OK: " ++ label)
  | otherwise = error
      (label ++ ": se esperaba " ++ show expected ++
       ", pero se obtuvo " ++ show actual)

dynamicProgram :: ASA
dynamicProgram =
  App (Fun "x"
    (App (Fun "y"
      (App (Fun "x" (Id "y")) (Add (Num 3) (Num 3))))
      (Add (Id "x") (Num 2))))
    (Add (Num 2) (Num 2))

restorationProgram :: ASA
restorationProgram =
  App (Fun "x"
    (Add (App (Fun "x" (Id "x")) (Num 1)) (Id "x")))
    (Num 10)

main :: IO ()
main = do
  assertEqual "alcance dinamico accidental" (Num 8)
    (interp dynamicProgram [])
  assertEqual "argumento sin usar" (Num 1)
    (interp (App (Fun "x" (Num 1)) (Id "libre")) [])
  assertEqual "restauracion del ambiente" (Num 11)
    (interp restorationProgram [])
  assertEqual "resta truncada" (Num 0)
    (interp (Sub (Num 2) (Num 5)) [])
