module InterpEnvDin where

import Desugar

type Env = [(String, ASA)]

smallStep :: ASA -> Env -> (ASA, Env)
smallStep (Id i) env = (lookupEnv i env, env)
smallStep (Num n) env = (Num n, env)
smallStep (Boolean b) env = (Boolean b, env)
smallStep (Add (Num n) (Num m)) env = (Num (n + m), env)
smallStep (Add (Num n) d) env =
  let (d', env') = smallStep d env
   in (Add (Num n) d', env')
smallStep (Add d1 d2) env =
  let (d1', env') = smallStep d1 env
   in (Add d1' d2, env')
smallStep (Sub (Num n) (Num m)) env = (Num (n - m), env)
smallStep (Sub (Num n) d) env =
  let (d', env') = smallStep d env
   in (Sub (Num n) d', env')
smallStep (Sub d1 d2) env =
  let (d1', env') = smallStep d1 env
   in (Sub d1' d2, env')
smallStep (Not (Boolean b)) env = (Boolean (not b), env)
smallStep (Not d) env =
  let (d', env') = smallStep d env
   in (Not d', env')
smallStep (Fun p c) env = (Fun p c, env)
smallStep (App (Fun p c) a) env
  | isValue a = (c, (p, a) : env)
  | otherwise =
    let (a', env') = smallStep a env
     in (App (Fun p c) a', env')
smallStep (App f a) env =
  let (f', env') = smallStep f env
   in (App f' a, env')

interp :: ASA -> Env -> ASA
interp e env
  | isValue e = e
  | otherwise =
    let (e', env') = smallStep e env
     in interp e' env'
    
lookupEnv :: String -> Env -> ASA
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j = v
  | otherwise = lookupEnv i env

isValue :: ASA -> Bool
isValue (Num _) = True
isValue (Boolean _) = True
isValue (Fun _ _) = True
isValue _ = False

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

funP :: ASA -> String
funP (Fun p _) = p

funC :: ASA -> ASA
funC (Fun _ c) = c