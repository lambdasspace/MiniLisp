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
  deriving (Show)

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
