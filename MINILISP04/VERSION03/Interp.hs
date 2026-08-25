module Interp where

import Desugar

type Env = [(String, ASAValues)]

-- Evaluador perezoso de paso grande. Un parámetro se liga a una cerradura de
-- expresión que conserva el argumento y el ambiente de la llamada. No hay
-- caché: cada demanda vuelve a evaluar la expresión.
interp :: ASAValues -> Env -> ASAValues
interp (IdV identifier) env = force (lookupEnv identifier env)
interp value@(NumV _) _ = value
interp value@(BooleanV _) _ = value
interp (AddV left right) env =
  NumV (numN (interp left env) + numN (interp right env))
interp (SubV left right) env =
  NumV (max 0 (numN (interp left env) - numN (interp right env)))
interp (NotV expression) env =
  BooleanV (not (boolN (interp expression env)))
interp (If0V condition consequent alternative) env =
  case interp condition env of
    NumV 0 -> interp consequent env
    NumV _ -> interp alternative env
    result -> error ("Se esperaba un número en if0: " ++ show result)
interp (FunV parameter body) env = ClosureV parameter body env
interp closure@(ClosureV _ _ _) _ = closure
interp expressionClosure@(ExprV _ _) _ = force expressionClosure
interp (AppV function argument) callerEnv =
  case interp function callerEnv of
    ClosureV parameter body definitionEnv ->
      interp body ((parameter, delay argument callerEnv) : definitionEnv)
    result -> error ("Se esperaba una cerradura: " ++ show result)

delay :: ASAValues -> Env -> ASAValues
delay = ExprV

force :: ASAValues -> ASAValues
force (ExprV expression savedEnv) = interp expression savedEnv
force value = value

runProgram :: ASA -> ASAValues
runProgram e = interp (desugarV e) []

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j = v
  | otherwise = lookupEnv i env

numN :: ASAValues -> Int
numN (NumV n) = n
numN expression = error ("Se esperaba un número: " ++ show expression)

boolN :: ASAValues -> Bool
boolN (BooleanV b) = b
boolN (NumV _) = True
boolN expression =
  error ("Se esperaba un booleano o número: " ++ show expression)

closureP :: ASAValues -> String
closureP (ClosureV p _ _) = p
closureP expression = error ("Se esperaba una cerradura: " ++ show expression)

closureC :: ASAValues -> ASAValues
closureC (ClosureV _ c _) = c
closureC expression = error ("Se esperaba una cerradura: " ++ show expression)

closureE :: ASAValues -> Env
closureE (ClosureV _ _ e) = e
closureE expression = error ("Se esperaba una cerradura: " ++ show expression)
