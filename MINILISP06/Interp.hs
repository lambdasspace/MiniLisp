module Interp where

import Desugar

type Env = [(String, Value)]

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV (Value -> (Value -> Value) -> Value)
  | ContV (Value -> Value)

instance Eq Value where
  NumV x == NumV y = x == y
  BooleanV x == BooleanV y = x == y
  _ == _ = False

instance Show Value where
  show (NumV n) = show n
  show (BooleanV b) = show b
  show (ClosureV _) = "<procedure>"
  show (ContV _) = "<continuation>"

{-
ASA = Id String
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | If0 ASA ASA ASA
  | LetCC String ASA
  | Fun String ASA
  | App ASA ASA-}

interp :: ASA -> Env -> (Value -> Value) -> Value
interp (Id i)      e k = k (lookupEnv i e)
interp (Num n)     _ k = k (NumV n)
interp (Boolean b) _ k = k (BooleanV b)
interp (Add i d)   e k = interp i e (\iv -> interp d e (\dv -> k (NumV (numN iv + numN dv))))
interp (Sub i d)   e k = interp i e (\iv -> interp d e (\dv -> k (NumV (numN iv - numN dv))))
interp (Not b)     e k = interp b e (\bv -> k (BooleanV (boolN bv)))
interp (If0 c a b) e k = interp c e (\cv -> if numN cv == 0 then interp a e k else interp b e k) 
interp (LetCC i c) e k = interp c ((i, ContV k) : e) k
interp (Fun p c)   e k = k (ClosureV (\av dk -> interp c ((p, av) : e) dk))
interp (App f a)   e k = interp f e (\fv -> interp a e (\av -> case fv of
                                                                ClosureV c -> c av k
                                                                ContV c    -> c av
                                                                _          -> error "not an applicable value"))

lookupEnv :: String -> Env -> Value
lookupEnv i [] = error ("Variable libre: " ++ i)
lookupEnv i ((j, v) : xs)
  | i == j    = v
  | otherwise = lookupEnv i xs

numN :: Value -> Int
numN (NumV n) = n
numN _        = error "Expected a numeric value"

boolN :: Value -> Bool
boolN (BooleanV b) = b
boolN _            = error "Expected a boolean value"
