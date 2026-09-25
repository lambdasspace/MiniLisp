module Main where

import Desugar
import Interp

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = putStrLn ("OK: " ++ label)
  | otherwise = error
      (label ++ ": se esperaba " ++ show expected ++
       ", pero se obtuvo " ++ show actual)

evalProgram :: ASA -> ASAValues
evalProgram expression = strict (interp (desugarV expression) [])

staticProgram :: ASA
staticProgram =
  App (Fun "x"
    (App (Fun "y"
      (App (Fun "x" (Id "y")) (Add (Num 3) (Num 3))))
      (Add (Id "x") (Num 2))))
    (Add (Num 2) (Num 2))

conditionalProgram :: ASA
conditionalProgram =
  App (Fun "a"
    (App (Fun "b"
      (App (Fun "a" (If0 (Id "b") (Num 1) (Num 2)))
           (Sub (Num 4) (Num 4))))
      (Add (Id "a") (Id "a"))))
    (Add (Num 4) (Num 4))

restorationProgram :: ASA
restorationProgram =
  App (Fun "x"
    (Add (App (Fun "x" (Id "x")) (Num 1)) (Id "x")))
    (Num 10)

main :: IO ()
main = do
  assertEqual "alcance estatico" (NumV 6)
    (evalProgram staticProgram)
  assertEqual "if0 y puntos estrictos" (NumV 2)
    (evalProgram conditionalProgram)
  assertEqual "argumento sin usar" (NumV 4)
    (evalProgram (App (Fun "x" (Num 4)) (Id "libre")))
  assertEqual "resta truncada" (NumV 0)
    (evalProgram (Sub (Num 2) (Num 5)))
  assertEqual "negacion booleana" (BooleanV False)
    (evalProgram (Not (Boolean True)))
  assertEqual "restauracion del ambiente" (NumV 11)
    (evalProgram restorationProgram)

  let argument = AddV (NumV 2) (NumV 3)
      delayed = ExprV argument []
      delayedEnv = [("x", delayed)]
      body = AddV (IdV "x") (IdV "x")
  assertEqual "Id devuelve la cerradura" delayed
    (interp (IdV "x") delayedEnv)
  assertEqual "primera demanda" (NumV 5)
    (strict (interp (IdV "x") delayedEnv))
  assertEqual "segunda demanda vuelve a evaluar" (NumV 5)
    (strict (interp (IdV "x") delayedEnv))
  assertEqual "la ligadura no se actualiza" delayed
    (lookupEnv "x" delayedEnv)
