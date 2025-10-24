module Interp where

import Desugar

type Env = [(String, ASAValues)]

smallStep :: ASAValues -> Env -> (ASAValues, Env)
smallStep (IdV i) env                                   = (lookupEnv i env, env)
smallStep (NumV n) env                                  = (NumV n, env)
smallStep (BooleanV b) env                              = (BooleanV b, env)
smallStep (AddV (NumV n) (NumV m)) env                  = (NumV (n + m), env)
smallStep (AddV (NumV n) ev@(ExprV _ _)) env            = (AddV (NumV n) (stepStrict ev), env) 
smallStep (AddV (ExprV (NumV n) env1) d) env            = (AddV (NumV n) (fst (smallStep d env)), env) 
smallStep (AddV (NumV n) d) env                         = (AddV (NumV n) (fst (smallStep d env)), env)
smallStep (AddV ev@(ExprV _ _) d) env                   = (AddV (stepStrict ev) d, env)
smallStep (AddV i d) env                                = (AddV (fst (smallStep i env)) d, env)
smallStep (SubV (NumV n) (NumV m)) env                  = (NumV (n + m), env)
smallStep (SubV (NumV n) ev@(ExprV _ _)) env            = (SubV (NumV n) (stepStrict ev), env) 
smallStep (SubV (ExprV (NumV n) env1) d) env            = (SubV (NumV n) (fst (smallStep d env)), env) 
smallStep (SubV (NumV n) d) env                         = (SubV (NumV n) (fst (smallStep d env)), env)
smallStep (SubV ev@(ExprV _ _) d) env                   = (SubV (stepStrict ev) d, env)
smallStep (SubV i d) env                                = (SubV (fst (smallStep i env)) d, env)
smallStep (NotV (BooleanV b))   env                     = (BooleanV (not b), env)
smallStep (NotV ev@(ExprV _ _)) env                     = (NotV (stepStrict ev), env)
smallStep (NotV b) env                                  = (NotV (fst (smallStep b env)), env)
smallStep (If0V (NumV 0) t e) env                       = (t,env)
smallStep (If0V (NumV n) t e) env                       = (e,env)
smallStep (If0V ev@(ExprV _ _) t e) env                 = (If0V (stepStrict ev) t e, env)
smallStep (If0V c t e) env                              = (If0V (fst (smallStep c env)) t e, env)
smallStep (FunV p c) env                                = (ClosureV p c env, env)
smallStep (AppV (ClosureV p b env') a) env              = (b, (p,ExprV a env):env')
smallStep (AppV ev@(ExprV _ _) a) env                   = (AppV (stepStrict ev) a, env)
smallStep (AppV f a) env                                = (AppV (fst (smallStep f env)) a, env)

stepStrict :: ASAValues -> ASAValues
stepStrict (ExprV e env)
  | isValueV e = e 
  | otherwise = ExprV (fst (smallStep e env)) env

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
