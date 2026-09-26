# MiniLisp

Manuel Soto Romero

This repository documents the incremental design and implementation of **MiniLisp**, a pedagogical programming language conceived to explore fundamental concepts in programming language theory. A version may add expressive capabilities or provide a different semantic mechanism for an existing language.

The project follows the pedagogical approach of the **first edition of *Programming Languages: Application and Interpretation* (PLAI)** by Shriram Krishnamurthi, while using **Haskell** as the host language. The choice of Haskell provides a precise functional framework for expressing interpreters and semantic definitions, reinforcing theoretical connections to substitution, evaluation strategies, and higher-order functions.

MiniLisp uses **natural operational semantics** as its default executable model: each interpreter relates a complete expression to its final value. Structural operational semantics is retained only where intermediate transitions are themselves the object of study. Version 1 implements both styles so that their results can be compared explicitly; versions 2 through 5 use direct evaluators that mirror their big-step specifications. Versions 6 and 7 expose the control of execution because machines and first-class continuations are their respective objects of study.

## Repository Contents

### Arithmetic and Boolean Expressions

* [MiniLisp v1](MINILISP01): Arithmetic and boolean expressions, with both big-step and small-step evaluators for an explicit comparison of the two styles.

### Variables and Substitution

* [MiniLisp v2](MINILISP02): Adds identifiers and `let` expressions with substitution-based binding and direct eager evaluation.

### Functions and Scope

* [MiniLisp v3.1](MINILISP03/VERSION01): Anonymous functions with eager, substitution-based big-step evaluation.
* [MiniLisp v3.2](MINILISP03/VERSION02): Anonymous functions with a direct environment evaluator and dynamic scope.
* [MiniLisp v3.3](MINILISP03/VERSION03): Anonymous functions with closures and static scope.

### Evaluation Strategies

* [MiniLisp v4.1](MINILISP04/VERSION01): Big-step substitution semantics with deferred function arguments.
* [MiniLisp v4.2](MINILISP04/VERSION02): A deliberately naive big-step environment model that stores raw argument expressions and therefore exhibits accidental dynamic scope.
* [MiniLisp v4.3](MINILISP04/VERSION03): Big-step evaluation with expression closures, static scope, `if0`, and strictness points.

### Recursion

* [MiniLisp v5.1](MINILISP05/version01): Recursive definitions via `letrec`, desugared through the Y Combinator under lazy evaluation.
* [MiniLisp v5.2](MINILISP05/version02): Recursive definitions via `letrec`, desugared through the Z Combinator under eager evaluation.

### Explicit Control

* [MiniLisp v6](MINILISP06): A CEK machine for an eager, statically scoped MiniLisp. Its control stack is internal and cannot be manipulated by MiniLisp programs.

### Continuations

* [MiniLisp v7](MINILISP07): First-class continuations through the `let/cc` construct.

## Purpose

The overarching aim of this repository is to provide a **didactic progression** from simple arithmetic constructs to advanced control operators. Through these successive versions, students and researchers are invited to analyze:

* the correspondence between syntax and semantics,
* substitution and environment models for variable binding,
* distinctions between static and dynamic scope,
* strict versus lazy evaluation strategies,
* formal treatments of recursion, and
* the expressive role of continuations.

By adapting the **PLAI methodology to Haskell**, this project serves both as a **teaching tool** in computer science curricula and as a **research framework** for the study of programming language foundations.
