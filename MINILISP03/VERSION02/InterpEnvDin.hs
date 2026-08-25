module InterpEnvDin where

import Desugar

type Env = [(String, ASA)]

-- Evaluador de paso grande con ambientes y alcance dinámico. Una función no
-- conserva el ambiente de su definición; el cuerpo se evalúa extendiendo el
-- ambiente de la llamada.
interp :: ASA -> Env -> ASA
interp (Id identifier) env = lookupEnv identifier env
interp value@(Num _) _ = value
interp value@(Boolean _) _ = value
interp (Add left right) env =
  Num (numN (interp left env) + numN (interp right env))
interp (Sub left right) env =
  Num (max (numN (interp left env) - numN (interp right env)) 0)
interp (Not expression) env =
  Boolean (not (boolN (interp expression env)))
interp function@(Fun _ _) _ = function
interp (App function argument) env =
  case interp function env of
    Fun parameter body ->
      let value = interp argument env
      in interp body ((parameter, value) : env)
    result -> error ("Se esperaba una función: " ++ show result)

lookupEnv :: String -> Env -> ASA
lookupEnv identifier [] = error ("Variable libre: " ++ identifier)
lookupEnv identifier ((name, value) : env)
  | identifier == name = value
  | otherwise = lookupEnv identifier env

numN :: ASA -> Int
numN (Num number) = number
numN expression = error ("Se esperaba un número: " ++ show expression)

boolN :: ASA -> Bool
boolN (Boolean boolean) = boolean
boolN (Num _) = True
boolN expression =
  error ("Se esperaba un booleano o número: " ++ show expression)

funP :: ASA -> String
funP (Fun parameter _) = parameter
funP expression = error ("Se esperaba una función: " ++ show expression)

funC :: ASA -> ASA
funC (Fun _ body) = body
funC expression = error ("Se esperaba una función: " ++ show expression)
