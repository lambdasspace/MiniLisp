module Interp where

import Grammars

smallStep :: ASA -> ASA
smallStep (Id i) = error "Variable libre"
smallStep (Num n) = (Num n)
smallStep (Boolean b) = (Boolean b)
smallStep (Add (Num i) (Num d)) = Num (i + d)
smallStep (Add (Num i) d) = Add (Num i) (smallStep d)
smallStep (Add i d) = Add (smallStep i) d
smallStep (Sub (Num i) (Num d)) = Num (i - d)
smallStep (Sub (Num i) d) = Sub (Num i) (smallStep d)
smallStep (Sub i d) = Sub (smallStep i) d
smallStep (Not (Boolean b)) = Boolean (not b)
smallStep (Not e) = Not (smallStep e)
smallStep (Let i v b)
  | esValor v = sust b i v
  | otherwise = Let i (smallStep v) b

interp :: ASA -> ASA
interp (Id i) = error "Variable libre"
interp (Num n) = (Num n)
interp (Boolean b) = (Boolean b)
interp e = interp (smallStep e)

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

esValor :: ASA -> Bool
esValor (Num _) = True
esValor (Boolean _) = True
esValor _ = False

sust :: ASA -> String -> ASA -> ASA
sust (Num n) i v = Num n
sust (Boolean b) i v = Boolean b
sust (Id i') i v = if i' == i then v else (Id i')
sust (Add i' d) i v = Add (sust i' i v) (sust d i v)
sust (Sub i' d) i v = Sub (sust i' i v) (sust d i v)
sust (Not e) i v = Not (sust e i v)
sust (Let i' v' c) i v
  | i' == i = Let i' (sust v' i v) c
  | i' /= i = Let i' (sust v' i v) (sust c i v)