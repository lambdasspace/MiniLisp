module Interp where

import Desugar
import qualified Data.Set as Set

-- Evaluación por nombre mediante sustitución: la aplicación sustituye la
-- expresión del argumento sin evaluarla. Cada uso vuelve a evaluarla.
interp :: ASA -> ASA
interp (Id identifier) = error ("Variable libre: " ++ identifier)
interp value@(Num _) = value
interp value@(Boolean _) = value
interp (Add left right) =
  Num (numN (interp left) + numN (interp right))
interp (Sub left right) =
  Num (max 0 (numN (interp left) - numN (interp right)))
interp (Not expression) = Boolean (not (boolN (interp expression)))
interp function@(Fun _ _) = function
interp (App function argument) =
  interpApp (interp function) argument

interpApp :: ASA -> ASA -> ASA
interpApp (Fun parameter body) argument =
  interp (sust body parameter argument)

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN (Num _) = True

funP :: ASA -> String
funP (Fun p _) = p

funC :: ASA -> ASA
funC (Fun _ c) = c

sust :: ASA -> String -> ASA -> ASA
sust (Num n) i v = Num n
sust (Boolean b) i v = Boolean b
sust (Id i') i v = if i' == i then v else (Id i')
sust (Add i' d) i v = Add (sust i' i v) (sust d i v)
sust (Sub i' d) i v = Sub (sust i' i v) (sust d i v)
sust (Not e) i v = Not (sust e i v)
sust (Fun p c) i v
  | i == p = Fun p c
  | p `Set.member` freeVars v =
      let p' = freshName p (Set.unions [freeVars c, freeVars v, Set.singleton i])
          c' = sust c p (Id p')
       in Fun p' (sust c' i v)
  | otherwise = Fun p (sust c i v)
sust (App f a) i v = App (sust f i v) (sust a i v)

freeVars :: ASA -> Set.Set String
freeVars (Num _) = Set.empty
freeVars (Boolean _) = Set.empty
freeVars (Id i) = Set.singleton i
freeVars (Add i d) = Set.union (freeVars i) (freeVars d)
freeVars (Sub i d) = Set.union (freeVars i) (freeVars d)
freeVars (Not e) = freeVars e
freeVars (Fun p c) = Set.delete p (freeVars c)
freeVars (App f a) = Set.union (freeVars f) (freeVars a)

freshName :: String -> Set.Set String -> String
freshName base used = choose 0
  where
    choose n
      | candidate `Set.notMember` used = candidate
      | otherwise = choose (n + 1)
      where
        candidate = base ++ "_" ++ show n
