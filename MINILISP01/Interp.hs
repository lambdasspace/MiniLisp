module Interp where

import Grammars

interp :: ASA -> ASA
interp (Num n) = (Num n)
interp (Boolean b) = (Boolean b)
interp (Add i d) = Num ((numN (interp i)) + (numN (interp d)))
interp (Sub i d) = Num ((numN (interp i)) - (numN (interp d)))
interp (Not e) = Boolean (not (boolN (interp e)))

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False