module Interp where

import Desugar

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
smallStep (Fun p c) = Fun p c
smallStep (App (Fun p c) a) = sust c p a

isValue :: ASA -> Bool
isValue (Num n) = True
isValue (Boolean b) = True
isValue (Fun p c) = True
isValue _ = False

interp :: ASA -> ASA
interp e
    | isValue e = e
    | otherwise = interp (smallStep e)

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

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
sust (Fun p c) i v = if i == p then Fun p c else Fun p (sust c i v)
sust (App f a) i v = App (sust f i v) (sust a i v)