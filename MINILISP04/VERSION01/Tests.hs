module Main where

import qualified Data.Set as Set
import Desugar
import Interp

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = putStrLn ("OK: " ++ label)
  | otherwise = error
      (label ++ ": se esperaba " ++ show expected ++
       ", pero se obtuvo " ++ show actual)

main :: IO ()
main = do
  assertEqual "argumento sin usar" (Num 1)
    (interp (App (Fun "x" (Num 1)) (Id "libre")))
  assertEqual "argumento diferido" (Num 8)
    (interp (App (Fun "x" (Add (Id "x") (Id "x")))
                 (Add (Num 2) (Num 2))))
  assertEqual "resta truncada" (Num 0)
    (interp (Sub (Num 2) (Num 5)))
  let result = interp
        (App (Fun "x" (Fun "y" (Id "x"))) (Id "y"))
  case result of
    Fun parameter body -> do
      assertEqual "renombramiento alfa" False (parameter == "y")
      assertEqual "variable libre preservada" (Set.singleton "y")
        (freeVars body)
    _ -> error "Se esperaba una funcion tras la sustitucion"
