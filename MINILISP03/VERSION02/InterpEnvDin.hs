module InterpEnvDin where

import Desugar

type Env = [(String, ASA)]

interp :: ASA -> Env -> ASA
interp (Id i) env = lookupEnv i env
interp (Num n) env = (Num n)
interp (Boolean b) env = (Boolean b)
interp (Add i d) env = Num ((numN (interp i env)) + (numN (interp d env)))
interp (Sub i d) env = Num ((numN (interp i env)) - (numN (interp d env)))
interp (Not e) env = Boolean (not (boolN (interp e env)))
interp (Fun p c) env = Fun p c
interp (App f a) env =
  let funVal = interp f env
   in interp (funC funVal) (((funP funVal), (interp a env)) : env)

lookupEnv :: String -> Env -> ASA
lookupEnv i [] = error ("Variable libre: " ++ i)
lookupEnv i ((j, v) : xs)
  | i == j = v
  | otherwise = lookupEnv i xs

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

funP :: ASA -> String
funP (Fun p _) = p

funC :: ASA -> ASA
funC (Fun _ c) = c