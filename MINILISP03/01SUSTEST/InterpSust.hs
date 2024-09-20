module InterpSust where

import Desugar

interp :: ASA -> ASA
interp (Id i) = error "Variable libre"
interp (Num n) = (Num n)
interp (Boolean b) = (Boolean b)
interp (Add i d) = Num ((numN (interp i)) + (numN (interp d)))
interp (Sub i d) = Num ((numN (interp i)) - (numN (interp d)))
interp (Not e) = Boolean (not (boolN (interp e)))
interp (Fun p c) = Fun p c
interp (App f a) = let funVal = interp f in
                      interp (sust (funC funVal) (funP funVal) (interp a))

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