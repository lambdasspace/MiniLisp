module InterpSust where

import qualified Data.Set as Set
import Desugar

-- La ausencia de resultado indica que la expresión no puede dar un paso.
-- Esto incluye tanto los valores como las expresiones bloqueadas; interp
-- distingue ambos casos mediante isValue.
smallStep :: ASA -> Maybe ASA
smallStep (Id _) = Nothing
smallStep (Num _) = Nothing
smallStep (Boolean _) = Nothing
smallStep (Add (Num left) (Num right)) =
  Just (Num (left + right))
smallStep (Add (Num left) right) =
  Add (Num left) <$> smallStep right
smallStep (Add left right) =
  (`Add` right) <$> smallStep left
smallStep (Sub (Num left) (Num right)) =
  Just (Num (max (left - right) 0))
smallStep (Sub (Num left) right) =
  Sub (Num left) <$> smallStep right
smallStep (Sub left right) =
  (`Sub` right) <$> smallStep left
smallStep (Not (Boolean value)) =
  Just (Boolean (not value))
smallStep (Not (Num _)) =
  Just (Boolean False)
smallStep (Not expression) =
  Not <$> smallStep expression
smallStep (Fun _ _) = Nothing
smallStep (App (Fun parameter body) argument)
  | isValue argument = Just (sust body parameter argument)
smallStep (App function argument)
  | isValue function = App function <$> smallStep argument
smallStep (App function argument) =
  (`App` argument) <$> smallStep function

isValue :: ASA -> Bool
isValue (Num _) = True
isValue (Boolean _) = True
isValue (Fun _ _) = True
isValue _ = False

interp :: ASA -> ASA
interp expression
  | isValue expression = expression
  | otherwise =
      case smallStep expression of
        Just expression' -> interp expression'
        Nothing -> error ("Expresión bloqueada: " ++ show expression)

numN :: ASA -> Int
numN (Num n) = n
numN expression = error ("Se esperaba un número: " ++ show expression)

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

funP :: ASA -> String
funP (Fun p _) = p
funP expression = error ("Se esperaba una función: " ++ show expression)

funC :: ASA -> ASA
funC (Fun _ c) = c
funC expression = error ("Se esperaba una función: " ++ show expression)

freeVars :: ASA -> Set.Set String
freeVars (Id identifier) = Set.singleton identifier
freeVars (Num _) = Set.empty
freeVars (Boolean _) = Set.empty
freeVars (Add left right) = Set.union (freeVars left) (freeVars right)
freeVars (Sub left right) = Set.union (freeVars left) (freeVars right)
freeVars (Not expression) = freeVars expression
freeVars (Fun parameter body) = Set.delete parameter (freeVars body)
freeVars (App function argument) =
  Set.union (freeVars function) (freeVars argument)

allNames :: ASA -> Set.Set String
allNames (Id identifier) = Set.singleton identifier
allNames (Num _) = Set.empty
allNames (Boolean _) = Set.empty
allNames (Add left right) = Set.union (allNames left) (allNames right)
allNames (Sub left right) = Set.union (allNames left) (allNames right)
allNames (Not expression) = allNames expression
allNames (Fun parameter body) = Set.insert parameter (allNames body)
allNames (App function argument) =
  Set.union (allNames function) (allNames argument)

freshName :: Set.Set String -> String
freshName used = choose (0 :: Int)
  where
    choose index =
      let candidate = "fresh" ++ show index
      in if Set.member candidate used
           then choose (index + 1)
           else candidate

sust :: ASA -> String -> ASA -> ASA
sust (Num number) _ _ = Num number
sust (Boolean boolean) _ _ = Boolean boolean
sust (Id identifier) variable replacement
  | identifier == variable = replacement
  | otherwise = Id identifier
sust (Add left right) variable replacement =
  Add (sust left variable replacement) (sust right variable replacement)
sust (Sub left right) variable replacement =
  Sub (sust left variable replacement) (sust right variable replacement)
sust (Not expression) variable replacement =
  Not (sust expression variable replacement)
sust abstraction@(Fun parameter body) variable replacement
  | parameter == variable = abstraction
  | Set.notMember parameter (freeVars replacement) =
      Fun parameter (sust body variable replacement)
  | otherwise =
      let used = Set.unions
            [allNames body, allNames replacement,
             Set.fromList [parameter, variable]]
          fresh = freshName used
          renamedBody = sust body parameter (Id fresh)
      in Fun fresh (sust renamedBody variable replacement)
sust (App function argument) variable replacement =
  App (sust function variable replacement)
      (sust argument variable replacement)
