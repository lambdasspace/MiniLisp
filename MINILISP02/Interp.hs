module Interp where

import Grammars

-- Evaluador directo correspondiente a la semántica natural de la Nota 08.
-- En let se evalúa primero la expresión ligada y se sustituye su valor en el
-- cuerpo: ésta es una estrategia ansiosa.
interp :: ASA -> ASA
interp (Id identifier) =
  error ("Variable libre: " ++ identifier)
interp value@(Num _) = value
interp value@(Boolean _) = value
interp (Add left right) =
  Num (numN (interp left) + numN (interp right))
interp (Sub left right) =
  Num (max (numN (interp left) - numN (interp right)) 0)
interp (Not expression) =
  Boolean (not (boolN (interp expression)))
interp (Let identifier named body) =
  let value = interp named
  in interp (sust body identifier value)

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

-- sust e x s sustituye las apariciones libres de x por s en e.
sust :: ASA -> String -> ASA -> ASA
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b
sust (Id y) x s
  | y == x = s
  | otherwise = Id y
sust (Add e1 e2) x s = Add (sust e1 x s) (sust e2 x s)
sust (Sub e1 e2) x s = Sub (sust e1 x s) (sust e2 x s)
sust (Not e) x s = Not (sust e x s)
sust (Let y e1 e2) x s
  | y == x = Let y (sust e1 x s) e2
  | y `notElem` freeVars s =
      Let y (sust e1 x s) (sust e2 x s)
  | otherwise =
      let z = freshName (names e1 ++ names e2 ++ names s ++ [x, y])
          e2' = sust e2 y (Id z)
       in Let z (sust e1 x s) (sust e2' x s)

freeVars :: ASA -> [String]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (Id x) = [x]
freeVars (Add e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Sub e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Not e) = freeVars e
freeVars (Let x e1 e2) = freeVars e1 ++ filter (/= x) (freeVars e2)

names :: ASA -> [String]
names (Num _) = []
names (Boolean _) = []
names (Id x) = [x]
names (Add e1 e2) = names e1 ++ names e2
names (Sub e1 e2) = names e1 ++ names e2
names (Not e) = names e
names (Let x e1 e2) = x : names e1 ++ names e2

freshName :: [String] -> String
freshName used = choose 0
  where
    choose n
      | candidate `elem` used = choose (n + 1)
      | otherwise = candidate
      where
        candidate = "_x" ++ show (n :: Int)
