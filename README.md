# jppml

Interpreter for a simple SML-inspired functional language.

```ml
let partition = fn p l ->
  let part = fn yes no l -> case l of
    | [] -> (rev yes, rev no)
    | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l
  in
  part [] [] l
```

## Compiling

Make sure you have GHC version 9.0.2 or lower. Code generated by BNFC does not compile on new versions (at least for me).

```bash
cabal run
```

### Using BNFC to rebuild grammar

```bash
bnfc -m --functor Syntax.cf
make
```

### Running tests

This does not run examples from the `bad/` directory. They shouldn't be doing anything more thorough than the tests in `test/` though.

```bash
cabal test
```

## Usage

Use `./interpreter` without arguments to enter REPL (recommended, prints things in color and can display information about types). Or `./interpreter <file>` to interpret code from a file.

## Source code

The preprocessor aggresively desugars language constructs into function applications, for example even if-then-else is replaced with `__if` of type `bool -> (unit -> 'a) -> (unit -> 'a) -> 'a`. There is a minimal set of built-in functions not implemented in JPPML itself.

Source code uses identifiers prefixed with "__" for its reserved values, especially for functions that correspond to built-in operators. They can be defined only in let declarations/expressions and can't be redefined later. Type names and constructor names can't be redefined either.

The typechecker checks for missing symbols, illegal bindings, and does SML-style type reconstruction. You can see types of values in the interactive interpreter. The typechecker tries its best to report useful errors, with position of the error included in error messages.

The preprocessor and the typechecker allow interpreter to be relatively small and do relatively little. Values are stored indirectly, with type `Ptr` used as location designator and `Val` as actual value.

### Files

- [Syntax.cf](src/Syntax.cf) and auto-generated AbsSyntax.hs, LexSyntax.hs, LexSyntax.x, ParSyntax.hs, ParSyntax.y, PrintSyntax.hs, SkelSyntax.hs – syntax of the language and lexer, parser, utilities auto-generated by BNFC
- [Preprocess.hs](src/Preprocess.hs) – preprocessor. Simplifies AST, desugars code
- [Typecheck.hs](src/Typecheck.hs) – type checker
- [Infer.hs](src/Typecheck.hs) – type inference for the type checker, does type inference
- [Eval.hs](src/Eval.hs) – interpreter
- [app/Main.hs](app/Main.hs) – entry point for the executable
- [Core.hs](src/Core.hs) – built-in values for the interpreter
- [Core.ml](src/Core.ml) – standard library. Includes functions that the interpreter depends on
- [List.ml](src/List.ml) – standard `List` module, also a great example of JPPML code
- [good/](good/), [bad/](bad/) – folders with examples of correct and incorrect code
- [test/Main.hs](test/Main.hs) and others in [test/](test/) – files with many small tests, and source code for the program to run all tests

## Cennik

`+` = jest zaimplementowane

```
  Na 20 punktów
+ 01 (dwa typy)
+ 02 (arytmetyka, porównania)
+ 03 (if)
+ 04 (funkcje wieloargumentowe, rekurencja)
+ 05 (funkcje anonimowe i wyższego rzędu, częściowa aplikacja)
+ 06 (obsługa błędów wykonania)
+ 07 (statyczne wiązanie identyfikatorów)
  Listy:
+ 08 (z pattern matchingiem)
+ 09 (z empty, head, tail)
+ 10 (lukier)
  Na 25 punktów
+ 11 (listy dowolnego typu, zagnieżdżone i listy funkcji)
+ 12 (proste typy algebraiczne z jednopoziomowym pattern matchingiem)
+ 13 (statyczne typowanie)
  Na 30 punktów
+ 14 (ogólne polimorficzne i rekurencyjne typy algebraiczne)
+ 15 (zagnieżdżony pattern matching)
  Bonus
+ 16 (typy polimorficzne z algorytmem rekonstrukcji typów)
  17 (sprawdzenie kompletności pattern matchingu)

Razem: 34
```
