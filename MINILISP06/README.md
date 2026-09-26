# MiniLisp v6: máquina abstracta CEK

Esta versión utiliza evaluación ansiosa, alcance estático y ambientes
recursivos. La diferencia está en el
mecanismo de ejecución: `Interp.hs` reemplaza el evaluador directo por una
máquina de transiciones pequeñas.

El tipo `Stack` representa la pila interna de control. No es un valor de
MiniLisp: un programa no puede nombrarla, capturarla ni aplicarla. Las
continuaciones de primera clase y `let/cc` pertenecen a MiniLisp v7.

## Componentes

- `Lex.x` y `Grammars.y`: sintaxis superficial de esta versión.
- `Desugar.hs`: núcleo ejecutable. `let` permanece explícito para corresponder
  con el marco `LetK` de la máquina.
- `Interp.hs`: estados, marcos, transición `step`, trazas y ejecución completa.
- `MiniLisp.hs`: REPL y funciones para ejecutar o imprimir una traza.
- `Tests.hs`: pruebas de resultados, alcance, recursión, bloqueos y divergencia.

## Operaciones principales

- `step`: realiza una transición.
- `traceMachine`: produce la secuencia de estados desde una configuración.
- `execute`: ejecuta desde una expresión y un ambiente.
- `runMachine`: ejecuta un programa cerrado.
- `interp`: conserva la interfaz del evaluador directo para comparar
  resultados con el evaluador directo.

Desde `MiniLisp.hs`, `runSource` recibe código fuente y devuelve un resultado,
mientras que `printTrace` imprime sus estados.

## Compilación y pruebas

```text
alex Lex.x
happy --ghc Grammars.y
ghc -package array Tests.hs -o tests
./tests
```

En GHCi:

```text
ghci -package array MiniLisp.hs
> testConductor
15
> testSuma
6
> printTrace "(+ 1 2)"
```
