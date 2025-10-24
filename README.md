# MiniLisp

Manuel Soto Romero

This repository documents the incremental design and implementation of **MiniLisp**, a pedagogical programming language conceived to explore fundamental concepts in programming language theory. Each version introduces new expressive capabilities, ranging from arithmetic and boolean expressions to continuations.

The project follows the pedagogical approach of the **first edition of *Programming Languages: Application and Interpretation* (PLAI)** by Shriram Krishnamurthi, while using **Haskell** as the host language. The choice of Haskell provides a precise functional framework for expressing interpreters and semantic definitions, reinforcing theoretical connections to substitution, evaluation strategies, and higher-order functions.

Another **substantial difference** with respect to *PLAI* is that **MiniLisp adopts a structural operational semantics** instead of a natural semantics. This choice emphasizes the step-by-step reduction process of program evaluation, making explicit the intermediate computation states and transitions that occur during execution. Such an approach facilitates the study of **small-step semantics** and its relationship to implementation techniques such as abstract machines.

## Repository Contents

### Arithmetic and Boolean Expressions

* [MiniLisp v1](MINILISP01) — Arithmetic and boolean expressions with binary operators.

### Variables and Substitution

* [MiniLisp v2](MINILISP02) — `let` expressions with substitution-based binding.

### Functions and Scope

* [MiniLisp v3.1](MINILISP03/VERSION01) — Anonymous functions with substitution under static scope.
* [MiniLisp v3.2](MINILISP03/VERSION02) — Anonymous functions with environments under dynamic scope.
* [MiniLisp v3.3](MINILISP03/VERSION03) — Anonymous functions with environments under static scope.

### Evaluation Strategies

* [MiniLisp v4.1](MINILISP04/VERSION01) — Substitution semantics with lazy evaluation.
* [MiniLisp v4.2](MINILISP04/VERSION02) — Environment-based semantics with lazy evaluation.
* [MiniLisp v4.3](MINILISP04/VERSION03) — `if0` conditional with environments, lazy evaluation, and strictness points.

### Recursion

* [MiniLisp v5](MINILISP05) — Recursive definitions via `letrec`, implemented through the Y Combinator.

### Continuations

* [MiniLisp v6](MINILISP06) — First-class continuations through the `let/cc` construct.

## Purpose

The overarching aim of this repository is to provide a **didactic progression** from simple arithmetic constructs to advanced control operators. Through these successive versions, students and researchers are invited to analyze:

* the correspondence between syntax and semantics,
* substitution and environment models for variable binding,
* distinctions between static and dynamic scope,
* strict versus lazy evaluation strategies,
* formal treatments of recursion, and
* the expressive role of continuations.

By adapting the **PLAI methodology to Haskell**, this project serves both as a **teaching tool** in computer science curricula and as a **research framework** for the study of programming language foundations.
