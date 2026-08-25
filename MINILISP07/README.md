# MiniLisp v7: continuaciones de primera clase

Esta versión conserva la sintaxis, la evaluación ansiosa, el alcance estático,
los ambientes recursivos y la máquina CEK de MiniLisp v6. La única extensión
del lenguaje es `let/cc`, que captura la pila de control vigente y la guarda en
un valor `ContV`.

Aplicar una cerradura conserva la pila del punto de aplicación. Aplicar un
valor `ContV` descarta esa pila e instala la que fue capturada. Ambas operaciones
usan la sintaxis de aplicación de MiniLisp, pero tienen reglas de transición
distintas.

## Componentes

- `Lex.x` y `Grammars.y`: lenguaje completo de v6 más `let/cc`.
- `Desugar.hs`: sintaxis abstracta, valores, marcos y `ContV`.
- `Interp.hs`: estados y transiciones de la máquina CEK.
- `MiniLisp.hs`: funciones para ejecutar programas e imprimir trazas.
- `Tests.hs`: pruebas de captura, descarte, reinstalación y compatibilidad con
  la recursión de v6.

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
> testLetCC
7
> testEscape
7
> printTrace "(+ 1 (let/cc k (+ 100 (k 3))))"
```
