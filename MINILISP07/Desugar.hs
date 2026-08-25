module Desugar where

import Grammars

type Env = [(String, ASAValues)]

-- Árbol de sintaxis abstracta del núcleo ejecutable.
-- let permanece explícito porque la máquina tiene un marco LetK.
data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Mul ASA ASA
  | Leq ASA ASA
  | Not ASA
  | If0 ASA ASA ASA
  | If ASA ASA ASA
  | Let String ASA ASA
  | LetRec String ASA ASA
  | LetCC String ASA
  | Fun String ASA
  | App ASA ASA
  deriving (Eq, Show)

-- Expresiones del núcleo y valores producidos durante la ejecución.
data ASAValues
  = IdV String
  | NumV Int
  | BooleanV Bool
  | AddV ASAValues ASAValues
  | SubV ASAValues ASAValues
  | MulV ASAValues ASAValues
  | LeqV ASAValues ASAValues
  | NotV ASAValues
  | If0V ASAValues ASAValues ASAValues
  | IfV ASAValues ASAValues ASAValues
  | LetV String ASAValues ASAValues
  | LetRecV String ASAValues ASAValues
  | LetCCV String ASAValues
  | FunV String ASAValues
  | ClosureV String ASAValues Env
  | ContV Stack
  | AppV ASAValues ASAValues
  deriving Eq

-- La pila sigue siendo la estructura de control de la CEK. En v7 también
-- puede quedar almacenada dentro de un valor ContV.
data Stack
  = Mt
  | AddL ASAValues Env Stack
  | AddR Int Stack
  | SubL ASAValues Env Stack
  | SubR Int Stack
  | MulL ASAValues Env Stack
  | MulR Int Stack
  | LeqL ASAValues Env Stack
  | LeqR Int Stack
  | NotK Stack
  | If0K ASAValues ASAValues Env Stack
  | IfK ASAValues ASAValues Env Stack
  | LetK String ASAValues Env Stack
  | FunK ASAValues Env Stack
  | ArgK ASAValues Stack
  deriving (Eq, Show)

instance Show ASAValues where
  show (IdV name) = name
  show (NumV number) = show number
  show (BooleanV True) = "#t"
  show (BooleanV False) = "#f"
  show (AddV left right) = "(+ " ++ show left ++ " " ++ show right ++ ")"
  show (SubV left right) = "(- " ++ show left ++ " " ++ show right ++ ")"
  show (MulV left right) = "(* " ++ show left ++ " " ++ show right ++ ")"
  show (LeqV left right) = "(<= " ++ show left ++ " " ++ show right ++ ")"
  show (NotV expression) = "(not " ++ show expression ++ ")"
  show (If0V condition yes no) =
    "(if0 " ++ show condition ++ " " ++ show yes ++ " " ++ show no ++ ")"
  show (IfV condition yes no) =
    "(if " ++ show condition ++ " " ++ show yes ++ " " ++ show no ++ ")"
  show (LetV name value body) =
    "(let (" ++ name ++ " " ++ show value ++ ") " ++ show body ++ ")"
  show (LetRecV name value body) =
    "(letrec (" ++ name ++ " " ++ show value ++ ") " ++ show body ++ ")"
  show (LetCCV name body) =
    "(let/cc " ++ name ++ " " ++ show body ++ ")"
  show (FunV parameter body) =
    "(lambda (" ++ parameter ++ ") " ++ show body ++ ")"
  show (ClosureV _ _ _) = "#<procedure>"
  show (ContV _) = "#<continuation>"
  show (AppV function argument) =
    "(" ++ show function ++ " " ++ show argument ++ ")"

-- Traducción de la sintaxis superficial al núcleo.
desugar :: SASA -> ASA
desugar (IdS name) = Id name
desugar (NumS number) = Num number
desugar (BooleanS boolean) = Boolean boolean
desugar (AddS left right) = Add (desugar left) (desugar right)
desugar (SubS left right) = Sub (desugar left) (desugar right)
desugar (MulS left right) = Mul (desugar left) (desugar right)
desugar (LeqS left right) = Leq (desugar left) (desugar right)
desugar (NotS expression) = Not (desugar expression)
desugar (If0S condition yes no) =
  If0 (desugar condition) (desugar yes) (desugar no)
desugar (IfS condition yes no) =
  If (desugar condition) (desugar yes) (desugar no)
desugar (LetS name value body) =
  Let name (desugar value) (desugar body)
desugar (LetRecS name value body) =
  LetRec name (desugar value) (desugar body)
desugar (LetCCS name body) =
  LetCC name (desugar body)
desugar (FunS parameter body) =
  Fun parameter (desugar body)
desugar (AppS function argument) =
  App (desugar function) (desugar argument)

-- Traducción al tipo que consume la máquina.
desugarV :: ASA -> ASAValues
desugarV (Id name) = IdV name
desugarV (Num number) = NumV number
desugarV (Boolean boolean) = BooleanV boolean
desugarV (Add left right) = AddV (desugarV left) (desugarV right)
desugarV (Sub left right) = SubV (desugarV left) (desugarV right)
desugarV (Mul left right) = MulV (desugarV left) (desugarV right)
desugarV (Leq left right) = LeqV (desugarV left) (desugarV right)
desugarV (Not expression) = NotV (desugarV expression)
desugarV (If0 condition yes no) =
  If0V (desugarV condition) (desugarV yes) (desugarV no)
desugarV (If condition yes no) =
  IfV (desugarV condition) (desugarV yes) (desugarV no)
desugarV (Let name value body) =
  LetV name (desugarV value) (desugarV body)
desugarV (LetRec name value body) =
  LetRecV name (desugarV value) (desugarV body)
desugarV (LetCC name body) =
  LetCCV name (desugarV body)
desugarV (Fun parameter body) =
  FunV parameter (desugarV body)
desugarV (App function argument) =
  AppV (desugarV function) (desugarV argument)
