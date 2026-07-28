module Interp where

import Grammars

-- Conserva las transiciones de MINILISP01 y añade identificadores y let.
-- Nothing significa que no existe una transición saliente.
smallStep :: ASA -> Maybe ASA
smallStep (Id _) = Nothing
smallStep (Num _) = Nothing
smallStep (Boolean _) = Nothing
smallStep (Add (Num i) (Num d)) = Just (Num (i + d))
smallStep (Add (Num i) d) = Add (Num i) <$> smallStep d
smallStep (Add i d) = (`Add` d) <$> smallStep i
smallStep (Sub (Num i) (Num d)) = Just (Num (max (i - d) 0))
smallStep (Sub (Num i) d) = Sub (Num i) <$> smallStep d
smallStep (Sub i d) = (`Sub` d) <$> smallStep i
smallStep (Not (Boolean False)) = Just (Boolean True)
smallStep (Not (Boolean True)) = Just (Boolean False)
smallStep (Not (Num _)) = Just (Boolean False)
smallStep (Not e) = Not <$> smallStep e
smallStep (Let i v b)
  | esValor v = Just (sust b i v)
  | otherwise = (\v' -> Let i v' b) <$> smallStep v

interp :: ASA -> ASA
interp e
  | esValor e = e
  | otherwise =
      case smallStep e of
        Just e' -> interp e'
        Nothing -> error "Expresión bloqueada"

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

esValor :: ASA -> Bool
esValor (Num _) = True
esValor (Boolean _) = True
esValor _ = False

-- sust e x s sustituye las apariciones libres de x por s en e.
sust :: ASA -> String -> ASA -> ASA
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b
sust (Id y) x s
  | y == x = s
  | otherwise = Id y
sust (Add e1 e2) x s = Add (sust e1 x s) (sust e2 x s)
sust (Sub e1 e2) x s = Sub (sust e1 x s) (sust e2 x s)
sust (Not e) x s = Not (sust e x s)
sust (Let y e1 e2) x s
  | y == x = Let y (sust e1 x s) e2
  | y `notElem` freeVars s =
      Let y (sust e1 x s) (sust e2 x s)
  | otherwise =
      let z = freshName (names e1 ++ names e2 ++ names s ++ [x, y])
          e2' = sust e2 y (Id z)
       in Let z (sust e1 x s) (sust e2' x s)

freeVars :: ASA -> [String]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (Id x) = [x]
freeVars (Add e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Sub e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Not e) = freeVars e
freeVars (Let x e1 e2) = freeVars e1 ++ filter (/= x) (freeVars e2)

names :: ASA -> [String]
names (Num _) = []
names (Boolean _) = []
names (Id x) = [x]
names (Add e1 e2) = names e1 ++ names e2
names (Sub e1 e2) = names e1 ++ names e2
names (Not e) = names e
names (Let x e1 e2) = x : names e1 ++ names e2

freshName :: [String] -> String
freshName used = choose 0
  where
    choose n
      | candidate `elem` used = choose (n + 1)
      | otherwise = candidate
      where
        candidate = "_x" ++ show (n :: Int)
