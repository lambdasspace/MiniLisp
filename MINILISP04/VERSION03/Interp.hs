module Interp where

import Desugar

type Env = [(String, ASAValues)]

smallStep :: ASAValues -> Env -> (ASAValues, Env)
smallStep (IdV i) env                                   = (lookupEnv i env, env)
smallStep (NumV n) env                                  = (NumV n, env)
smallStep (BooleanV b) env                              = (BooleanV b, env)
smallStep (AddV (NumV n) (ExprV (NumV m) env2)) env     = (NumV (n + m), env)
smallStep (AddV (NumV n) (NumV m)) env                  = (NumV (n + m), env)
smallStep (AddV (NumV n) (ExprV e2 env2)) env           = (AddV (NumV n) (fst (smallStep e2 env2)), env) 
smallStep (AddV (ExprV (NumV n) env1) d) env            = (AddV (NumV n) (fst (smallStep d env)), env) 
smallStep (AddV (NumV n) d) env                         = (AddV (NumV n) (fst (smallStep d env)), env)
smallStep (AddV (ExprV e1 env1) d) env                  = (AddV (fst (smallStep e1 env1)) d, env)
smallStep (AddV i d) env                                = (AddV (fst (smallStep i env)) d, env)
smallStep (SubV (NumV n) (ExprV (NumV m) env2)) env     = (NumV (n + m), env)
smallStep (SubV (NumV n) (NumV m)) env                  = (NumV (n + m), env)
smallStep (SubV (NumV n) (ExprV e2 env2)) env           = (SubV (NumV n) (fst (smallStep e2 env2)), env) 
smallStep (SubV (ExprV (NumV n) env1) d) env            = (SubV (NumV n) (fst (smallStep d env)), env) 
smallStep (SubV (NumV n) d) env                         = (SubV (NumV n) (fst (smallStep d env)), env)
smallStep (SubV (ExprV e1 env1) d) env                  = (SubV (fst (smallStep e1 env1)) d, env)
smallStep (SubV i d) env                                = (SubV (fst (smallStep i env)) d, env)
smallStep (NotV (ExprV (BooleanV b) env1)) env          = (BooleanV (not b), env)
smallStep (NotV (BooleanV b))   env                     = (BooleanV (not b), env)
smallStep (NotV (ExprV e1 env1)) env                    = (NotV (fst (smallStep e1 env1)), env)
smallStep (NotV b) env                                  = (NotV (fst (smallStep b env)), env)
smallStep (If0V (ExprV (NumV 0) env1) t e) env          = (t,env)
smallStep (If0V (NumV 0) t e) env                       = (t,env)
smallStep (If0V (ExprV (NumV n) env1) t e) env          = (e,env)
smallStep (If0V (NumV n) t e) env                       = (e,env)
smallStep (If0V (ExprV c env1) t e) env                 = (If0V (fst (smallStep c env1)) t e, env)
smallStep (If0V c t e) env                              = (If0V (fst (smallStep c env)) t e, env)
smallStep (FunV p c) env                                = (ClosureV p c env, env)
smallStep (AppV (ExprV (ClosureV p b env') env1) a) env = (b, (p,ExprV a env):env') 
smallStep (AppV (ClosureV p b env') a) env              = (b, (p,ExprV a env):env')
smallStep (AppV (ExprV f env1) a) env                   = (AppV (fst (smallStep f env1)) a, env)
smallStep (AppV f a) env                                = (AppV (fst (smallStep f env)) a, env)

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
