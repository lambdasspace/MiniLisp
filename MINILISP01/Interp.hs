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

-- Evaluación mediante la cerradura reflexiva-transitiva de smallStep. Se
-- conserva para comparar ambos estilos en la Nota 06.
interpSmall :: ASA -> ASA
interpSmall e
  | esValor e = e
  | otherwise =
      case smallStep e of
        Just e' -> interpSmall e'
        Nothing -> error "Expresión bloqueada"

-- Semántica natural: cada llamada recursiva corresponde a una premisa de la
-- regla de paso grande del constructor exterior.
bigStep :: ASA -> ASA
bigStep value@(Num _) = value
bigStep value@(Boolean _) = value
bigStep (Add left right) =
  Num (numN (bigStep left) + numN (bigStep right))
bigStep (Sub left right) =
  Num (max (numN (bigStep left) - numN (bigStep right)) 0)
bigStep (Not expression) =
  Boolean (not (boolN (bigStep expression)))

-- A partir de esta versión, interp nombra al evaluador directo del curso.
interp :: ASA -> ASA
interp = bigStep

-- Funciones auxiliares para extraer valores de ASA
numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN (Num _) = True
boolN expression =
  error ("Se esperaba un booleano o número: " ++ show expression)

esValor :: ASA -> Bool
esValor (Num _) = True
esValor (Boolean _) = True
esValor _ = False
