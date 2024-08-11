module Interp where

import Grammars

interp :: ASA -> ASA
interp (Id i) = error "Variable libre"
interp (Num n) = (Num n)
interp (Boo b) = (Boo b)
interp (Add i d) = Num ((numN (interp i)) + (numN (interp d)))
interp (Sub i d) = Num ((numN (interp i)) - (numN (interp d)))
interp (Not e) = Boo (not (boolN (interp e)))
interp (Let i v c) = interp (sust c i (interp v))

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boo b) = b
boolN _ = False

sust :: ASA -> String -> ASA -> ASA
sust (Num n) i v = Num n
sust (Boo b) i v = Boo b
sust (Id i') i v = if i' == i then v else (Id i')
sust (Add i' d) i v = Add (sust i' i v) (sust d i v)
sust (Sub i' d) i v = Sub (sust i' i v) (sust d i v)
sust (Not e) i v = Not (sust e i v)
sust (Let i' v' c) i v
  | i' == i = Let i' (sust v' i v) c
  | i' /= i = Let i' (sust v' i v) (sust c i v)