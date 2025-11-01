module Interp where

import Desugar

type Env = [(String, ASAValues)]

-- Importante: En esta versión del intérprete la combinación de evaluación perezosa, 
-- el combinador Y y la semántica de paso pequeño generan una situación particular: 
-- durante la evaluación, el cuerpo de una función puede contener variables (como ‘f’ 
-- en Y) que deben permanecer accesibles aunque aún no se hayan evaluado completamente. 
-- Para preservar esas ligaduras y evitar errores de variables libres, realizamos dos 
-- cambios importantes:

-- • Al aplicar una cerradura, el cuerpo se devuelve envuelto en una cerradura de expresión: 
--   ExprV b env''. Así garantizamos que la evaluación ocurra dentro
--   del ambiente extendido correcto (env''), manteniendo los enlaces necesarios
--   para la recursión del combinador Y.

-- • Dado que el cuerpo ahora se encuentra dentro de un ExprV, debemos permitir
--   que el sistema avance dentro de estas expresiones. Por ello añadimos la regla:
--       smallStep ev@(ExprV _ _) env = (stepStrict ev, env)
--   Esta regla permite forzar un paso dentro de ExprV, asegurando que la máquina
--   siga reduciendo de manera coherente bajo evaluación perezosa.


smallStep :: ASAValues -> Env -> (ASAValues, Env)
smallStep (IdV i) env                                   = (lookupEnv i env, env)
smallStep (NumV n) env                                  = (NumV n, env)
smallStep (BooleanV b) env                              = (BooleanV b, env)

-- Suma
smallStep (AddV (NumV n) (NumV m)) env                  = (NumV (n + m), env)
smallStep (AddV (NumV n) ev@(ExprV _ _)) env            = (AddV (NumV n) (stepStrict ev), env) 
smallStep (AddV (ExprV (NumV n) env1) d) env            = (AddV (NumV n) (fst (smallStep d env)), env) 
smallStep (AddV (NumV n) d) env                         = (AddV (NumV n) (fst (smallStep d env)), env)
smallStep (AddV ev@(ExprV _ _) d) env                   = (AddV (stepStrict ev) d, env)
smallStep (AddV i d) env                                = (AddV (fst (smallStep i env)) d, env)

-- Resta
smallStep (SubV (NumV n) (NumV m)) env                  = (NumV (n - m), env)
smallStep (SubV (NumV n) ev@(ExprV _ _)) env            = (SubV (NumV n) (stepStrict ev), env) 
smallStep (SubV (ExprV (NumV n) env1) d) env            = (SubV (NumV n) (fst (smallStep d env)), env) 
smallStep (SubV (NumV n) d) env                         = (SubV (NumV n) (fst (smallStep d env)), env)
smallStep (SubV ev@(ExprV _ _) d) env                   = (SubV (stepStrict ev) d, env)
smallStep (SubV i d) env                                = (SubV (fst (smallStep i env)) d, env)

-- Multiplicación
smallStep (MulV (NumV n) (NumV m)) env                  = (NumV (n * m), env)
smallStep (MulV (NumV n) ev@(ExprV _ _)) env            = (MulV (NumV n) (stepStrict ev), env)
smallStep (MulV (ExprV (NumV n) env1) d) env            = (MulV (NumV n) (fst (smallStep d env)), env)
smallStep (MulV (NumV n) d) env                         = (MulV (NumV n) (fst (smallStep d env)), env)
smallStep (MulV ev@(ExprV _ _) d) env                   = (MulV (stepStrict ev) d, env)
smallStep (MulV i d) env                                = (MulV (fst (smallStep i env)) d, env)

-- Comparación menor o igual
smallStep (LeqV (NumV n) (NumV m)) env                  = (BooleanV (n <= m), env)
smallStep (LeqV (NumV n) ev@(ExprV _ _)) env            = (LeqV (NumV n) (stepStrict ev), env)
smallStep (LeqV (ExprV (NumV n) env1) d) env            = (LeqV (NumV n) (fst (smallStep d env)), env)
smallStep (LeqV (NumV n) d) env                         = (LeqV (NumV n) (fst (smallStep d env)), env)
smallStep (LeqV ev@(ExprV _ _) d) env                   = (LeqV (stepStrict ev) d, env)
smallStep (LeqV i d) env                                = (LeqV (fst (smallStep i env)) d, env)

-- Not
smallStep (NotV (BooleanV b))   env                     = (BooleanV (not b), env)
smallStep (NotV ev@(ExprV _ _)) env                     = (NotV (stepStrict ev), env)
smallStep (NotV b) env                                  = (NotV (fst (smallStep b env)), env)

-- If
-- If (booleana)
smallStep (IfV (BooleanV True)  t e) env = (t, env)
smallStep (IfV (BooleanV False) t e) env = (e, env)
smallStep (IfV ev@(ExprV _ _) t e) env   = (IfV (stepStrict ev) t e, env)
smallStep (IfV c t e) env                = (IfV (fst (smallStep c env)) t e, env)

-- If0
smallStep (If0V (NumV 0) t e) env                       = (t,env)
smallStep (If0V (NumV n) t e) env                       = (e,env)
smallStep (If0V ev@(ExprV _ _) t e) env                 = (If0V (stepStrict ev) t e, env)
smallStep (If0V c t e) env                              = (If0V (fst (smallStep c env)) t e, env)

-- Funciones y aplicación
smallStep (FunV p c) env                                = (ClosureV p c env, env)
smallStep (AppV (ClosureV p b env') a) env              = 
  let env'' = (p, ExprV a env) : env'
  in (ExprV b env'', env'')
smallStep (AppV ev@(ExprV _ _) a) env                   = (AppV (stepStrict ev) a, env)
smallStep (AppV f a) env                                = (AppV (fst (smallStep f env)) a, env)

-- Delegar ExprV tope a stepStrict (para avanzar un paso interno)
smallStep ev@(ExprV _ _) env             = (stepStrict ev, env)

-- Paso estricto
stepStrict :: ASAValues -> ASAValues
stepStrict (ExprV e env)
  | isValueV e = e 
  | otherwise  = ExprV (fst (smallStep e env)) env

-- Interpretación total
interp :: ASAValues -> Env -> ASAValues
interp e env
  | isValueV e = e
  | otherwise  =
      let (e', env') = smallStep e env
       in interp e' env'

-- Predicados y auxiliares
isValueV :: ASAValues -> Bool
isValueV (NumV _)           = True
isValueV (BooleanV _)       = True
isValueV (ClosureV _ _ _)   = True
isValueV _                  = False

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j    = v
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
