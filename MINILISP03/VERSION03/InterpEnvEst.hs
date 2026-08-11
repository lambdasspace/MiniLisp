module InterpEnvEst where

import Desugar

type Env = [(String, ASAValues)]

-- La ausencia de resultado representa una configuración sin transición. Los
-- valores terminan; las demás configuraciones sin paso están bloqueadas.
smallStep :: ASAValues -> Env -> Maybe (ASAValues, Env)
smallStep (IdV identifier) env = do
  value <- lookupEnv identifier env
  pure (value, env)
smallStep (NumC number) env =
  Just (NumV number, env)
smallStep (BooleanC boolean) env =
  Just (BooleanV boolean, env)
smallStep (NumV _) _ = Nothing
smallStep (BooleanV _) _ = Nothing
smallStep (ClosureV _ _ _) _ = Nothing
smallStep (AddV (NumV left) (NumV right)) env =
  Just (NumV (left + right), env)
smallStep (AddV (NumV left) right) env = do
  (right', env') <- smallStep right env
  pure (AddV (NumV left) right', env')
smallStep (AddV left right) env = do
  (left', env') <- smallStep left env
  pure (AddV left' right, env')
smallStep (SubV (NumV left) (NumV right)) env =
  Just (NumV (max (left - right) 0), env)
smallStep (SubV (NumV left) right) env = do
  (right', env') <- smallStep right env
  pure (SubV (NumV left) right', env')
smallStep (SubV left right) env = do
  (left', env') <- smallStep left env
  pure (SubV left' right, env')
smallStep (NotV (BooleanV value)) env =
  Just (BooleanV (not value), env)
smallStep (NotV (NumV _)) env =
  Just (BooleanV False, env)
smallStep (NotV expression) env = do
  (expression', env') <- smallStep expression env
  pure (NotV expression', env')
smallStep (FunV parameter body) env =
  Just (ClosureV parameter body env, env)
smallStep (AppV (ClosureV parameter body definitionEnv) argument) callerEnv
  | isValueV argument =
      Just (RetV callerEnv (desugarV body),
            (parameter, argument) : definitionEnv)
smallStep (AppV function argument) env
  | isValueV function = do
      (argument', env') <- smallStep argument env
      pure (AppV function argument', env')
smallStep (AppV function argument) env = do
  (function', env') <- smallStep function env
  pure (AppV function' argument, env')
smallStep (RetV callerEnv body) env
  | isValueV body = Just (body, callerEnv)
  | otherwise = do
      (body', env') <- smallStep body env
      pure (RetV callerEnv body', env')

interp :: ASAValues -> Env -> ASAValues
interp expression env
  | isValueV expression = expression
  | otherwise = case smallStep expression env of
      Just (expression', env') -> interp expression' env'
      Nothing -> error ("Expresión bloqueada: " ++ show expression)

isValueV :: ASAValues -> Bool
isValueV (NumV _) = True
isValueV (BooleanV _) = True
isValueV (ClosureV _ _ _) = True
isValueV _ = False

lookupEnv :: String -> Env -> Maybe ASAValues
lookupEnv _ [] = Nothing
lookupEnv identifier ((name, value) : env)
  | identifier == name = Just value
  | otherwise = lookupEnv identifier env

numN :: ASAValues -> Int
numN (NumV number) = number
numN expression = error ("Se esperaba un número: " ++ show expression)

boolN :: ASAValues -> Bool
boolN (BooleanV boolean) = boolean
boolN expression = error ("Se esperaba un booleano: " ++ show expression)

closureP :: ASAValues -> String
closureP (ClosureV parameter _ _) = parameter
closureP expression = error ("Se esperaba una cerradura: " ++ show expression)

closureC :: ASAValues -> ASA
closureC (ClosureV _ body _) = body
closureC expression = error ("Se esperaba una cerradura: " ++ show expression)

closureE :: ASAValues -> Env
closureE (ClosureV _ _ env) = env
closureE expression = error ("Se esperaba una cerradura: " ++ show expression)
