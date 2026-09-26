module Desugar where

import Grammars

data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | If0 ASA ASA ASA
  | Fun String ASA
  | App ASA ASA
  deriving (Eq, Show)

data ASAValues
  = IdV String
  | NumV Int
  | BooleanV Bool
  | AddV ASAValues ASAValues
  | SubV ASAValues ASAValues
  | NotV ASAValues
  | If0V ASAValues ASAValues ASAValues
  | FunV String ASAValues
  | ClosureV String ASAValues [(String, ASAValues)]
  | AppV ASAValues ASAValues
  deriving (Eq, Show)

desugar :: SASA -> ASA
desugar (IdS identifier) = Id identifier
desugar (NumS number) = Num number
desugar (BooleanS boolean) = Boolean boolean
desugar (AddS left right) = Add (desugar left) (desugar right)
desugar (SubS left right) = Sub (desugar left) (desugar right)
desugar (NotS expression) = Not (desugar expression)
desugar (LetS parameter argument body) =
  App (Fun parameter (desugar body)) (desugar argument)
desugar (LetRecS name definition body) =
  desugar (LetS name (AppS (IdS "Z") (FunS name definition)) body)
desugar (If0S condition consequent alternative) =
  If0 (desugar condition) (desugar consequent) (desugar alternative)
desugar (FunS parameter body) = Fun parameter (desugar body)
desugar (AppS function argument) =
  App (desugar function) (desugar argument)

desugarV :: ASA -> ASAValues
desugarV (Id identifier) = IdV identifier
desugarV (Num number) = NumV number
desugarV (Boolean boolean) = BooleanV boolean
desugarV (Add left right) = AddV (desugarV left) (desugarV right)
desugarV (Sub left right) = SubV (desugarV left) (desugarV right)
desugarV (Not expression) = NotV (desugarV expression)
desugarV (If0 condition consequent alternative) =
  If0V (desugarV condition) (desugarV consequent) (desugarV alternative)
desugarV (Fun parameter body) = FunV parameter (desugarV body)
desugarV (App function argument) =
  AppV (desugarV function) (desugarV argument)
