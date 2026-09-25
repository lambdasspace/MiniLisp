module Interp where

import Desugar

type Env = [(String, ASA)]

-- Variante deliberadamente ingenua: las funciones sí son cerraduras, pero
-- el argumento se guarda como expresión desnuda. Cuando se consulta el
-- parámetro, esa expresión se evalúa en el ambiente del uso y sus variables
-- libres adquieren alcance dinámico accidental.
interp :: ASA -> Env -> ASA
interp (Id identifier) env =
  let stored = lookupEnv identifier env
  in if isValue stored then stored else interp stored env
interp value@(Num _) _ = value
interp value@(Boolean _) _ = value
interp (Add left right) env =
  Num (numN (interp left env) + numN (interp right env))
interp (Sub left right) env =
  Num (max 0 (numN (interp left env) - numN (interp right env)))
interp (Not expression) env = Boolean (not (boolN (interp expression env)))
interp (Fun parameter body) env = Closure parameter body env
interp closure@(Closure _ _ _) _ = closure
interp (App function argument) callerEnv =
  interpApp (interp function callerEnv) argument

interpApp :: ASA -> ASA -> ASA
interpApp (Closure parameter body definitionEnv) argument =
  interp body ((parameter, argument) : definitionEnv)
    
lookupEnv :: String -> Env -> ASA
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j = v
  | otherwise = lookupEnv i env

isValue :: ASA -> Bool
isValue (Num _) = True
isValue (Boolean _) = True
isValue (Closure _ _ _) = True
isValue _ = False

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN (Num _) = True

funP :: ASA -> String
funP (Closure p _ _) = p

funC :: ASA -> ASA
funC (Closure _ c _) = c
