module Interp where

import Grammars

-- Función que modela la semántica operacional estructural (->)
smallStep :: ASA -> ASA
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

-- Función que aprovecha la equivalencia entre la semántica estructural y natural
-- => equiv ->*
-- Es decir, aplica de forma iterativa la función smallStep hasta llegar a un ASA irreducible 
interp :: ASA -> ASA
interp (Num n) = (Num n)
interp (Boolean b) = (Boolean b)
interp e = interp (smallStep e)

-- Funciones auxiliares para extraer valores de ASA
numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False