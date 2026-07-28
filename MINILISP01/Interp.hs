module Interp where

import Grammars

-- Una transición de la semántica estructural. Nothing significa que no
-- existe una transición saliente.
smallStep :: ASA -> Maybe ASA
smallStep (Num _) = Nothing
smallStep (Boolean _) = Nothing
smallStep (Add (Num i) (Num d)) = Just (Num (i + d))
smallStep (Add (Num i) d) = Add (Num i) <$> smallStep d
smallStep (Add i d) = (`Add` d) <$> smallStep i
smallStep (Sub (Num i) (Num d)) = Just (Num (max (i - d) 0))
smallStep (Sub (Num i) d) = Sub (Num i) <$> smallStep d
smallStep (Sub i d) = (`Sub` d) <$> smallStep i
smallStep (Not (Boolean False)) = Just (Boolean True)
smallStep (Not (Boolean True)) = Just (Boolean False)
smallStep (Not (Num _)) = Just (Boolean False)
smallStep (Not e) = Not <$> smallStep e

-- Repite las transiciones hasta alcanzar un valor. Una forma normal que no
-- es un valor representa una expresión bloqueada.
interp :: ASA -> ASA
interp e
  | esValor e = e
  | otherwise =
      case smallStep e of
        Just e' -> interp e'
        Nothing -> error "Expresión bloqueada"

-- Funciones auxiliares para extraer valores de ASA
numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

esValor :: ASA -> Bool
esValor (Num _) = True
esValor (Boolean _) = True
esValor _ = False
