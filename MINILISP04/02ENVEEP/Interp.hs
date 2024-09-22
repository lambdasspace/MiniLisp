module Interp where

import Desugar

type Env = [(String, Value)]

data Value
  = NumV Int
  | BooleanV Bool
  | ExprV ASA Env
  | ClosureV String ASA Env

interp :: ASA -> Env -> Value
interp (Id i) env = lookupEnv i env
interp (Num n) env = (NumV n)
interp (Boolean b) env = (BooleanV b)
interp (Add i d) env = NumV ((numN (interp i env)) + (numN (interp d env)))
interp (Sub i d) env = NumV ((numN (interp i env)) - (numN (interp d env)))
interp (Not e) env = BooleanV (not (boolN (interp e env)))
interp (Fun p c) env = ClosureV p c env
interp (App f a) env =
  let funVal = interp f env
   in interp (closureC funVal) (((closureP funVal), ExprV a env) : (closureE funVal))

lookupEnv :: String -> Env -> Value
lookupEnv i [] = error ("Variable libre: " ++ i)
lookupEnv i ((j, v) : xs)
  | i == j = v
  | otherwise = lookupEnv i xs

numN :: Value -> Int
numN (NumV n) = n

boolN :: Value -> Bool
boolN (BooleanV b) = b
boolN _ = False

closureP :: Value -> String
closureP (ClosureV p _ _) = p

closureC :: Value -> ASA
closureC (ClosureV _ c _) = c

closureE :: Value -> Env
closureE (ClosureV _ _ e) = e