module Interp where

import Desugar

type Env = [(String, Value)]

data Value
  = NumV Int
  | BooleanV Bool
  | ExprV ASA Env
  | ClosureV String ASA Env
  deriving (Eq,Show)

interp :: ASA -> Env -> Value
interp (Id i) env = lookupEnv i env
interp (Num n) env = (NumV n)
interp (Boolean b) env = (BooleanV b)
interp (Add i d) env =
  let i' = (interp i env)
      d' = (interp d env)
   in NumV ((numN (strict i')) + (numN (strict d')))
interp (Sub i d) env =
  let i' = (interp i env)
      d' = (interp d env)
   in NumV ((numN (strict i')) - (numN (strict d')))
interp (Not e) env =
  let e' = (interp e env)
   in BooleanV (not (boolN (strict e')))
interp (If0 c t e) env =
  let c' = (interp c env)
      c'' = strict c'
   in if c'' == (NumV 0) then (interp t env) else (interp e env)
interp (Fun p c) env = ClosureV p c env
interp (App f a) env =
  let f' = interp f env
      funVal = strict f'
   in interp (closureC funVal) (((closureP funVal), ExprV a env) : (closureE funVal))

lookupEnv :: String -> Env -> Value
lookupEnv i [] = error ("Variable libre: " ++ i)
lookupEnv i ((j, v) : xs)
  | i == j = v
  | otherwise = lookupEnv i xs

strict :: Value -> Value
strict (NumV n) = NumV n
strict (BooleanV b) = BooleanV b
strict (ExprV a e) = strict (interp a e)
strict (ClosureV p c e) = ClosureV p c e

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