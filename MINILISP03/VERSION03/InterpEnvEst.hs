module InterpEnvEst where

import Desugar

type Env = [(String, ASAValues)]

-- Evaluador de paso grande con alcance estático. Una abstracción produce una
-- cerradura y la aplicación evalúa el cuerpo en el ambiente de definición.
interp :: ASA -> Env -> ASAValues
interp (Id identifier) env = lookupEnv identifier env
interp (Num number) _ = NumV number
interp (Boolean boolean) _ = BooleanV boolean
interp (Add left right) env =
  NumV (numN (interp left env) + numN (interp right env))
interp (Sub left right) env =
  NumV (max (numN (interp left env) - numN (interp right env)) 0)
interp (Not expression) env =
  BooleanV (not (boolN (interp expression env)))
interp (Fun parameter body) env = ClosureV parameter body env
interp (App function argument) env =
  case interp function env of
    ClosureV parameter body definitionEnv ->
      let value = interp argument env
      in interp body ((parameter, value) : definitionEnv)
    result -> error ("Se esperaba una cerradura: " ++ show result)

lookupEnv :: String -> Env -> ASAValues
lookupEnv identifier [] = error ("Variable libre: " ++ identifier)
lookupEnv identifier ((name, value) : env)
  | identifier == name = value
  | otherwise = lookupEnv identifier env

numN :: ASAValues -> Int
numN (NumV number) = number
numN expression = error ("Se esperaba un número: " ++ show expression)

boolN :: ASAValues -> Bool
boolN (BooleanV boolean) = boolean
boolN (NumV _) = True
boolN expression =
  error ("Se esperaba un booleano o número: " ++ show expression)

closureP :: ASAValues -> String
closureP (ClosureV parameter _ _) = parameter
closureP expression = error ("Se esperaba una cerradura: " ++ show expression)

closureC :: ASAValues -> ASA
closureC (ClosureV _ body _) = body
closureC expression = error ("Se esperaba una cerradura: " ++ show expression)

closureE :: ASAValues -> Env
closureE (ClosureV _ _ env) = env
closureE expression = error ("Se esperaba una cerradura: " ++ show expression)
