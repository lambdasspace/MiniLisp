module Interp where

import Lex
import Grammars
import Desugar

import System.IO (hFlush, stdout)

type Env = [(String, ASAValues)]

-- Usa un "hueco" sintáctico para los marcos
holeName :: String
holeName = "___HOLE____"

hole :: ASAValues
hole = IdV holeName

-- plug: rellena el hueco por un valor (sustitución estructural)
plug :: ASAValues -> ASAValues -> ASAValues
plug (IdV x)             v | x == holeName = v
plug (IdV x)             _                 = IdV x
plug (NumV n)            _                 = NumV n
plug (BooleanV b)        _                 = BooleanV b
plug (AddV l r)          v                 = AddV (plug l v) (plug r v)
plug (SubV l r)          v                 = SubV (plug l v) (plug r v)
plug (NotV x)            v                 = NotV (plug x v)
plug (If0V c t e)        v                 = If0V (plug c v) (plug t v) (plug e v)
plug (LetCCV p c)        v                 = LetCCV p (plug c v)
plug (FunV p c)          v                 = FunV p (plug c v)
plug (ClosureV p c e)    v                 = ClosureV p (plug c v) e
plug (AppV f a)          v                 = AppV (plug f v) (plug a v)
plug (ContV p (b,e,k))   v                 = ContV p (plug b v, e, k)
plug HaltV               _                 = HaltV

-- Paso pequeño con continuaciones que cargan un hueco (no un nombre en el env)
smallStep :: ASAValues -> Env -> ASAValues -> (ASAValues, Env, ASAValues)

-- Identificadores (el hueco nunca debe resolverse por ambiente)
smallStep (IdV i) e k
  | i == holeName          = error "Hueco sin rellenar en toplevel (bug del intérprete)"
  | otherwise              = (AppV k (lookupEnv i e), e, k)

-- Números y booleanos
smallStep (NumV n)     e k = (AppV k (NumV n), e, k)
smallStep (BooleanV b) e k = (AppV k (BooleanV b), e, k)

-- Sumas
smallStep (AddV (NumV i) (NumV d)) e k = (AppV k (NumV (i + d)), e, k)
smallStep (AddV iv@(NumV _) d)     e k = (d, e, ContV "_" (AddV iv hole, e, k))
smallStep (AddV i d)               e k = (i, e, ContV "_" (AddV hole d, e, k))

-- Restas
smallStep (SubV (NumV i) (NumV d)) e k = (AppV k (NumV (i - d)), e, k)
smallStep (SubV iv@(NumV _) d)     e k = (d, e, ContV "_" (SubV iv hole, e, k))
smallStep (SubV i d)               e k = (i, e, ContV "_" (SubV hole d, e, k))

-- Negación
smallStep (NotV (BooleanV b)) e k = (AppV k (BooleanV (not b)), e, k)
smallStep (NotV b)            e k = (b, e, ContV "_" (NotV hole, e, k))

-- If0
smallStep (If0V (NumV 0) t e2) e k = (t,  e, k)
smallStep (If0V (NumV _) t e2) e k = (e2, e, k)
smallStep (If0V c t e2)        e k = (c,  e, ContV "_" (If0V hole t e2, e, k))

-- let/cc: liga p := k en el ambiente actual
smallStep (LetCCV p c) e k = (c, (p,k):e, k)

-- Funciones y cierres
smallStep (FunV p c) e k = (AppV k (ClosureV p c e), e, k)

-- Aplicación
-- a cierre
smallStep (AppV (ClosureV p c e') a) e k
  | isValueV a = (c, (p,a):e', k)
  | otherwise  = (a, e, ContV "_" (AppV (ClosureV p c e') hole, e, k))

-- a continuación capturada: rellenar el hueco en su cuerpo y saltar a k'
smallStep (AppV (ContV _ (b,e',k')) a) e k
  | isValueV a = (plug b a, e', k')
  | otherwise  = (a, e, ContV "_" (AppV (ContV "_" (b,e',k')) hole, e, k))

-- continuación final
smallStep (AppV HaltV a) e k
  | isValueV a = (a, e, HaltV)
  | otherwise  = (a, e, ContV "_" (AppV HaltV hole, e, k))

-- evaluación por defecto: empuja un marco (□ a) para evaluar f primero
smallStep (AppV f a) e k = (f, e, ContV "_" (AppV hole a, e, k))

-- Interpretación paso a paso (con pausa)
interp :: ASAValues -> Env -> ASAValues -> ASAValues
interp e env k  --do
  -- Mostrar solo la expresión actual
  --putStrLn (show e)

  -- Esperar al usuario
  --putStr "Presiona Enter para continuar..."
  --hFlush stdout   -- Forzar que se imprima antes de esperar input
  --_ <- getLine

  | isValueV e = e
    --then pure e
  | otherwise =
      let (e1, env1, k1) = smallStep e env k
      in interp e1 env1 k1

-- Predicados y auxiliares
isValueV :: ASAValues -> Bool
isValueV (NumV _)           = True
isValueV (BooleanV _)       = True
isValueV (ClosureV _ _ _)   = True
isValueV (ContV _ _)        = True
isValueV HaltV              = True
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
