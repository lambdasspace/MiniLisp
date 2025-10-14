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

data ASAValues
  = IdV String
  | NumV Int
  | BooleanV Bool
  | AddV ASAValues ASAValues
  | SubV ASAValues ASAValues
  | NotV ASAValues
  | FunV String ASAValues
  | ClosureV String ASAValues [(String, ASAValues)]
  | AppV ASAValues ASAValues
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

desugarV :: ASA -> ASAValues
desugarV (Id i) = IdV i
desugarV (Num n) = NumV n
desugarV (Boolean b) = BooleanV b
desugarV (Add i d) = AddV (desugarV i) (desugarV d)
desugarV (Sub i d) = SubV (desugarV i) (desugarV d)
desugarV (Not e) = NotV (desugarV e)
desugarV (Fun p c) = FunV p (desugarV c)
desugarV (App f a) = AppV (desugarV f) (desugarV a)