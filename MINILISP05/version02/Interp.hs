module Interp where

import Desugar

type Env = [(String, ASAValues)]

-- Evaluador ansioso de paso grande con alcance estático.
interp :: ASAValues -> Env -> ASAValues
interp (IdV identifier) env = lookupEnv identifier env
interp value@(NumV _) _ = value
interp value@(BooleanV _) _ = value
interp (AddV left right) env =
  NumV (numN (interp left env) + numN (interp right env))
interp (SubV left right) env =
  NumV (max 0 (numN (interp left env) - numN (interp right env)))
interp (NotV expression) env =
  BooleanV (not (boolN (interp expression env)))
interp (If0V condition consequent alternative) env =
  interpIf0 (interp condition env) consequent alternative env
interp (FunV parameter body) env = ClosureV parameter body env
interp closure@(ClosureV _ _ _) _ = closure
interp (AppV function argument) env =
  interpApp (interp function env) (interp argument env)

interpIf0 :: ASAValues -> ASAValues -> ASAValues -> Env -> ASAValues
interpIf0 (NumV 0) consequent _ env = interp consequent env
interpIf0 (NumV _) _ alternative env = interp alternative env

interpApp :: ASAValues -> ASAValues -> ASAValues
interpApp (ClosureV parameter body definitionEnv) argument =
  interp body ((parameter, argument) : definitionEnv)

lookupEnv :: String -> Env -> ASAValues
lookupEnv identifier ((name, value) : env)
  | identifier == name = value
  | otherwise = lookupEnv identifier env
lookupEnv identifier [] =
  error ("Variable libre: " ++ identifier)

numN :: ASAValues -> Int
numN (NumV number) = number

boolN :: ASAValues -> Bool
boolN (BooleanV boolean) = boolean
