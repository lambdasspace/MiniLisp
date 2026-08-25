module Interp where

import Desugar

type Env = [(String, ASAValues)]

-- Evaluador directo (paso grande) con evaluación diferida sin almacenamiento.
-- Los argumentos se guardan como ExprV junto con el ambiente del sitio de
-- llamada y se fuerzan cada vez que se consultan.
interp :: ASAValues -> Env -> ASAValues
interp (IdV i) env = force (lookupEnv i env)
interp n@(NumV _) _ = n
interp b@(BooleanV _) _ = b
interp (AddV l r) env = NumV (expectNum (interp l env) + expectNum (interp r env))
interp (SubV l r) env = NumV (expectNum (interp l env) - expectNum (interp r env))
interp (MulV l r) env = NumV (expectNum (interp l env) * expectNum (interp r env))
interp (LeqV l r) env = BooleanV (expectNum (interp l env) <= expectNum (interp r env))
interp (NotV e) env = BooleanV (not (expectBool (interp e env)))
interp (If0V c t e) env =
  if expectNum (interp c env) == 0 then interp t env else interp e env
interp (IfV c t e) env =
  if expectBool (interp c env) then interp t env else interp e env
interp (FunV p body) env = ClosureV p body env
interp (AppV f a) env =
  case interp f env of
    ClosureV p body definitionEnv ->
      interp body ((p, ExprV a env) : definitionEnv)
    _ -> error "Application expects a function"
interp (ExprV e savedEnv) _ = interp e savedEnv

force :: ASAValues -> ASAValues
force (ExprV e savedEnv) = interp e savedEnv
force value = value

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, value) : env)
  | i == j = value
  | otherwise = lookupEnv i env

expectNum :: ASAValues -> Int
expectNum (NumV n) = n
expectNum _ = error "Numeric operation expects numbers"

expectBool :: ASAValues -> Bool
expectBool (BooleanV b) = b
expectBool _ = error "Boolean operation expects booleans"
