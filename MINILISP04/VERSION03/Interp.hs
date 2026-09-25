module Interp where

import Desugar

type Env = [(String, ASAValues)]

-- Evaluador perezoso de paso grande. Un parámetro se liga a una cerradura de
-- expresión que conserva el argumento y el ambiente de la llamada.
interp :: ASAValues -> Env -> ASAValues
interp (IdV identifier) env = lookupEnv identifier env
interp value@(NumV _) _ = value
interp value@(BooleanV _) _ = value
interp (AddV left right) env =
  NumV (numN (strict (interp left env)) + numN (strict (interp right env)))
interp (SubV left right) env =
  NumV (max 0 (numN (strict (interp left env)) - numN (strict (interp right env))))
interp (NotV expression) env =
  BooleanV (not (boolN (strict (interp expression env))))
interp (If0V condition consequent alternative) env =
  interpIf0 (strict (interp condition env)) consequent alternative env
interp (FunV parameter body) env = ClosureV parameter body env
interp closure@(ClosureV _ _ _) _ = closure
interp expressionClosure@(ExprV _ _) _ = expressionClosure
interp (AppV function argument) callerEnv =
  interpApp (strict (interp function callerEnv)) argument callerEnv

interpIf0 :: ASAValues -> ASAValues -> ASAValues -> Env -> ASAValues
interpIf0 (NumV 0) consequent _ env = interp consequent env
interpIf0 (NumV _) _ alternative env = interp alternative env

interpApp :: ASAValues -> ASAValues -> Env -> ASAValues
interpApp (ClosureV parameter body definitionEnv) argument callerEnv =
  interp body ((parameter, ExprV argument callerEnv) : definitionEnv)

strict :: ASAValues -> ASAValues
strict (ExprV expression savedEnv) = strict (interp expression savedEnv)
strict value = value

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j = v
  | otherwise = lookupEnv i env

numN :: ASAValues -> Int
numN (NumV n) = n

boolN :: ASAValues -> Bool
boolN (BooleanV b) = b
boolN (NumV _) = True

closureP :: ASAValues -> String
closureP (ClosureV p _ _) = p

closureC :: ASAValues -> ASAValues
closureC (ClosureV _ c _) = c

closureE :: ASAValues -> Env
closureE (ClosureV _ _ e) = e
