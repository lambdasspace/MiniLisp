module InterpEnvDin where

import Desugar

type Env = [(String, ASA)]

-- La ausencia de resultado significa que la configuración no puede dar un
-- paso. interp distingue los valores de las configuraciones bloqueadas.
smallStep :: ASA -> Env -> Maybe (ASA, Env)
smallStep (Id identifier) env = do
  value <- lookupEnv identifier env
  pure (value, env)
smallStep (Num _) _ = Nothing
smallStep (Boolean _) _ = Nothing
smallStep (Add (Num left) (Num right)) env =
  Just (Num (left + right), env)
smallStep (Add (Num left) right) env = do
  (right', env') <- smallStep right env
  pure (Add (Num left) right', env')
smallStep (Add left right) env = do
  (left', env') <- smallStep left env
  pure (Add left' right, env')
smallStep (Sub (Num left) (Num right)) env =
  Just (Num (max (left - right) 0), env)
smallStep (Sub (Num left) right) env = do
  (right', env') <- smallStep right env
  pure (Sub (Num left) right', env')
smallStep (Sub left right) env = do
  (left', env') <- smallStep left env
  pure (Sub left' right, env')
smallStep (Not (Boolean value)) env =
  Just (Boolean (not value), env)
smallStep (Not (Num _)) env =
  Just (Boolean False, env)
smallStep (Not expression) env = do
  (expression', env') <- smallStep expression env
  pure (Not expression', env')
smallStep (Fun _ _) _ = Nothing
smallStep (App (Fun parameter body) argument) env
  | isValue argument =
      Just (Ret env body, (parameter, argument) : env)
smallStep (App function argument) env
  | isValue function = do
      (argument', env') <- smallStep argument env
      pure (App function argument', env')
smallStep (App function argument) env = do
  (function', env') <- smallStep function env
  pure (App function' argument, env')
smallStep (Ret callerEnv body) env
  | isValue body = Just (body, callerEnv)
  | otherwise = do
      (body', env') <- smallStep body env
      pure (Ret callerEnv body', env')

interp :: ASA -> Env -> ASA
interp expression env
  | isValue expression = expression
  | otherwise = case smallStep expression env of
      Just (expression', env') -> interp expression' env'
      Nothing -> error ("Expresión bloqueada: " ++ show expression)

lookupEnv :: String -> Env -> Maybe ASA
lookupEnv _ [] = Nothing
lookupEnv identifier ((name, value) : env)
  | identifier == name = Just value
  | otherwise = lookupEnv identifier env

isValue :: ASA -> Bool
isValue (Num _) = True
isValue (Boolean _) = True
isValue (Fun _ _) = True
isValue _ = False

numN :: ASA -> Int
numN (Num number) = number
numN expression = error ("Se esperaba un número: " ++ show expression)

boolN :: ASA -> Bool
boolN (Boolean boolean) = boolean
boolN expression = error ("Se esperaba un booleano: " ++ show expression)

funP :: ASA -> String
funP (Fun parameter _) = parameter
funP expression = error ("Se esperaba una función: " ++ show expression)

funC :: ASA -> ASA
funC (Fun _ body) = body
funC expression = error ("Se esperaba una función: " ++ show expression)
