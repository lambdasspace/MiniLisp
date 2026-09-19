module InterpSust where

import qualified Data.Set as Set
import Desugar

-- Evaluador de paso grande con sustitución. La aplicación es ansiosa: tanto
-- la posición de función como el argumento se evalúan antes de sustituir.
interp :: ASA -> ASA
interp (Id identifier) =
  error ("Variable libre: " ++ identifier)
interp value@(Num _) = value
interp value@(Boolean _) = value
interp (Add left right) =
  Num (numN (interp left) + numN (interp right))
interp (Sub left right) =
  Num (max (numN (interp left) - numN (interp right)) 0)
interp (Not expression) =
  Boolean (not (boolN (interp expression)))
interp function@(Fun _ _) = function
interp (App function argument) =
  let functionValue = interp function
      parameter = funP functionValue
      body = funC functionValue
      argumentValue = interp argument
  in seq parameter
      (seq argumentValue
        (interp (sust body parameter argumentValue)))

numN :: ASA -> Int
numN (Num n) = n
numN expression = error ("Se esperaba un número: " ++ show expression)

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN (Num _) = True
boolN expression =
  error ("Se esperaba un booleano o número: " ++ show expression)

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
