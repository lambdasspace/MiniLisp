module Desugar where

import Grammars

data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun String ASA
  | App ASA ASA
  -- Forma interna de control. El analizador nunca produce Ret: la crea el
  -- evaluador al entrar al cuerpo de una función para recordar el ambiente
  -- de la invocación.
  | Ret [(String, ASA)] ASA
  deriving (Eq, Show)

desugar :: SASA -> ASA
desugar (IdS i) = Id i
desugar (NumS n) = Num n
desugar (BooleanS b) = Boolean b
desugar (AddS i d) = Add (desugar i) (desugar d)
desugar (SubS i d) = Sub (desugar i) (desugar d)
desugar (NotS e) = Not (desugar e)
desugar (LetS p v c) = App (Fun p (desugar c)) (desugar v)
desugar (FunS p c) = Fun p (desugar c)
desugar (AppS f a) = App (desugar f) (desugar a)
