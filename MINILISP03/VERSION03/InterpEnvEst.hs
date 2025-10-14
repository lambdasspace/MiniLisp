module InterpEnvEst where

import Desugar

type Env = [(String, ASAValues)]

smallStep :: ASAValues -> Env -> (ASAValues, Env)
smallStep (IdV i) env = (lookupEnv i env, env)
smallStep (NumV n) env = (NumV n, env)
smallStep (BooleanV b) env = (BooleanV b, env)
smallStep (AddV (NumV n) (NumV m)) env = (NumV (n + m), env)
smallStep (AddV (NumV n) d) env =
  let (d', env') = smallStep d env
   in (AddV (NumV n) d', env')
smallStep (AddV d1 d2) env =
  let (d1', env') = smallStep d1 env
   in (AddV d1' d2, env')
smallStep (SubV (NumV n) (NumV m)) env = (NumV (n - m), env)
smallStep (SubV (NumV n) d) env =
  let (d', env') = smallStep d env
   in (SubV (NumV n) d', env')
smallStep (SubV d1 d2) env =
  let (d1', env') = smallStep d1 env
   in (SubV d1' d2, env')
smallStep (NotV (BooleanV b)) env = (BooleanV (not b), env)
smallStep (NotV d) env =
  let (d', env') = smallStep d env
   in (NotV d', env')
smallStep (FunV p c) env = (ClosureV p c env, env)
smallStep (AppV (ClosureV p c e) a) env
  | isValueV a = (c, (p, a) : e)
  | otherwise =
    let (a', env') = smallStep a env
     in (AppV (ClosureV p c e) a', env')
smallStep (AppV f a) env =
  let (f', env') = smallStep f env
   in (AppV f' a, env')

interp :: ASAValues -> Env -> ASAValues
interp e env
  | isValueV e = e
  | otherwise =
    let (e', env') = smallStep e env
     in interp e' env'

isValueV :: ASAValues -> Bool
isValueV (NumV _) = True
isValueV (BooleanV _) = True
isValueV (ClosureV _ _ _) = True
isValueV _ = False

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j = v
  | otherwise = lookupEnv i env

numN :: ASAValues -> Int
numN (NumV n) = n

boolN :: ASAValues -> Bool
boolN (BooleanV b) = b
boolN _ = False

closureP :: ASAValues -> String
closureP (ClosureV p _ _) = p

closureC :: ASAValues -> ASAValues
closureC (ClosureV _ c _) = c

closureE :: ASAValues -> Env
closureE (ClosureV _ _ e) = e