module Interp where

import Desugar

-- CEK representa el momento de atender una expresión.
-- Return representa el momento de entregar un valor a la pila.
data State
  = CEK ASAValues Env Stack
  | Return Stack ASAValues
  | Stuck String
  deriving (Eq, Show)

step :: State -> Maybe State
step (CEK (IdV name) env stack) =
  case lookupValue name env of
    Just value -> Just (Return stack value)
    Nothing -> Just (Stuck ("Variable libre: " ++ name))

step (CEK value@(NumV _) _ stack) =
  Just (Return stack value)

step (CEK value@(BooleanV _) _ stack) =
  Just (Return stack value)

step (CEK (FunV parameter body) env stack) =
  Just (Return stack (ClosureV parameter body env))

step (CEK closure@(ClosureV _ _ _) _ stack) =
  Just (Return stack closure)

step (CEK continuation@(ContV _) _ stack) =
  Just (Return stack continuation)

step (CEK (AddV left right) env stack) =
  Just (CEK left env (AddL right env stack))

step (Return (AddL right env stack) (NumV left)) =
  Just (CEK right env (AddR left stack))

step (Return (AddR left stack) (NumV right)) =
  Just (Return stack (NumV (left + right)))

step (CEK (SubV left right) env stack) =
  Just (CEK left env (SubL right env stack))

step (Return (SubL right env stack) (NumV left)) =
  Just (CEK right env (SubR left stack))

step (Return (SubR left stack) (NumV right)) =
  Just (Return stack (NumV (left - right)))

step (CEK (MulV left right) env stack) =
  Just (CEK left env (MulL right env stack))

step (Return (MulL right env stack) (NumV left)) =
  Just (CEK right env (MulR left stack))

step (Return (MulR left stack) (NumV right)) =
  Just (Return stack (NumV (left * right)))

step (CEK (LeqV left right) env stack) =
  Just (CEK left env (LeqL right env stack))

step (Return (LeqL right env stack) (NumV left)) =
  Just (CEK right env (LeqR left stack))

step (Return (LeqR left stack) (NumV right)) =
  Just (Return stack (BooleanV (left <= right)))

step (CEK (NotV expression) env stack) =
  Just (CEK expression env (NotK stack))

step (Return (NotK stack) (BooleanV boolean)) =
  Just (Return stack (BooleanV (not boolean)))

step (CEK (If0V condition yes no) env stack) =
  Just (CEK condition env (If0K yes no env stack))

step (Return (If0K yes _ env stack) (NumV 0)) =
  Just (CEK yes env stack)

step (Return (If0K _ no env stack) (NumV _)) =
  Just (CEK no env stack)

step (CEK (IfV condition yes no) env stack) =
  Just (CEK condition env (IfK yes no env stack))

step (Return (IfK yes _ env stack) (BooleanV True)) =
  Just (CEK yes env stack)

step (Return (IfK _ no env stack) (BooleanV False)) =
  Just (CEK no env stack)

step (CEK (LetV name value body) env stack) =
  Just (CEK value env (LetK name body env stack))

step (Return (LetK name body env stack) value) =
  Just (CEK body ((name, value) : env) stack)

-- let/cc vuelve valor a la pila de control actual y la liga en el ambiente.
-- El cuerpo continúa bajo la misma pila.
step (CEK (LetCCV name body) env stack) =
  Just (CEK body ((name, ContV stack) : env) stack)

step (CEK (AppV function argument) env stack) =
  Just (CEK function env (FunK argument env stack))

step (Return (FunK argument env stack)
             closure@(ClosureV _ _ _)) =
  Just (CEK argument env (ArgK closure stack))

step (Return (FunK argument env stack)
             continuation@(ContV _)) =
  Just (CEK argument env (ArgK continuation stack))

step (Return (ArgK (ClosureV parameter body definitionEnv) stack)
             argument) =
  Just (CEK body ((parameter, argument) : definitionEnv) stack)

-- Aplicar una continuación capturada descarta la pila actual y reinstala
-- exactamente la pila guardada en el valor ContV.
step (Return (ArgK (ContV capturedStack) _) argument) =
  Just (Return capturedStack argument)

step (CEK (LetRecV name (FunV parameter functionBody) body)
          env stack) =
  let recursiveEnv = (name, recursiveClosure) : env
      recursiveClosure =
        ClosureV parameter functionBody recursiveEnv
  in Just (CEK body recursiveEnv stack)

step (CEK (LetRecV _ _ _) _ _) =
  Just (Stuck "letrec espera una funcion")

step (Return Mt _) =
  Nothing

step (Stuck _) =
  Nothing

step (Return (AddL _ _ _) _) =
  Just (Stuck "La suma espera numeros")

step (Return (AddR _ _) _) =
  Just (Stuck "La suma espera numeros")

step (Return (SubL _ _ _) _) =
  Just (Stuck "La resta espera numeros")

step (Return (SubR _ _) _) =
  Just (Stuck "La resta espera numeros")

step (Return (MulL _ _ _) _) =
  Just (Stuck "La multiplicacion espera numeros")

step (Return (MulR _ _) _) =
  Just (Stuck "La multiplicacion espera numeros")

step (Return (LeqL _ _ _) _) =
  Just (Stuck "La comparacion espera numeros")

step (Return (LeqR _ _) _) =
  Just (Stuck "La comparacion espera numeros")

step (Return (NotK _) _) =
  Just (Stuck "not espera un booleano")

step (Return (If0K _ _ _ _) _) =
  Just (Stuck "if0 espera un numero")

step (Return (IfK _ _ _ _) _) =
  Just (Stuck "if espera un booleano")

step (Return (FunK _ _ _) _) =
  Just (Stuck "La aplicacion espera una funcion o una continuacion")

step (Return (ArgK _ _) _) =
  Just (Stuck "Marco de aplicacion mal formado")

inject :: ASAValues -> State
inject expression =
  CEK expression [] Mt

injectWithEnv :: ASAValues -> Env -> State
injectWithEnv expression env =
  CEK expression env Mt

traceMachine :: State -> [State]
traceMachine state =
  state :
    case step state of
      Nothing -> []
      Just nextState -> traceMachine nextState

execute :: ASAValues -> Env -> Either String ASAValues
execute expression env =
  finish (injectWithEnv expression env)
  where
    finish state =
      case state of
        Return Mt value -> Right value
        Stuck message -> Left message
        _ ->
          case step state of
            Just nextState -> finish nextState
            Nothing -> Left "Estado final no reconocido"

runMachine :: ASAValues -> Either String ASAValues
runMachine expression =
  execute expression []

-- Conserva la interfaz de las versiones anteriores para facilitar
-- comparaciones de resultados con el evaluador directo.
interp :: ASAValues -> Env -> ASAValues
interp expression env =
  case execute expression env of
    Right value -> value
    Left message -> error message

lookupValue :: String -> Env -> Maybe ASAValues
lookupValue _ [] =
  Nothing
lookupValue name ((boundName, value) : env)
  | name == boundName = Just value
  | otherwise = lookupValue name env

lookupEnv :: String -> Env -> ASAValues
lookupEnv name env =
  case lookupValue name env of
    Just value -> value
    Nothing -> error ("Variable " ++ name ++ " no encontrada")
