# thkeme
___Scheme with a Lisp___

## Introduction
This project implements an REPL that interprets a
  very minimal subset of R5RS Scheme, in Haskell.

## Grammar
The below grammar is implemented using the Parsec library.

As one can see, the reason for using lisp as a first attempt at writing a compiler is because of the very simple grammar as can be seen before.

By using S-Expressions, with all functions necessarily structured in prefix notation i.e. (polish notation)
we eliminate the need to implement operator precedences, which simplifies the parsing by a lot.

```
letter      = [a-zA-Z0-9~!@#$%^&*()]
number      = [0-9]
numeric     = -? <number>* (. <number> *)?
bool        = #(f|t)
nil         = \'\(\)
str         = "<letter>*"
identStart  = <letter> | [!$%&*/:<=>?^_~]
identLetter = <number> | <letter> | [!$%&*/:<=>?^_~+-.@]
identifier  = <identStart> <identLetter> *

lispExpr    = 
    <nil>
  | <bool>
  | <str>
  | <identifier>     --> for references to functions
  | '<lispExpr>      --> for quoted expressions
  | ( <lispExpr> * ) --> S-Expressions

lispProgram = <lispExpr> *
```

## General Evaluation Scheme
`var` returns the value stored variable with the name "var"

`literal` where literal can be the basic types(numbers, characters, strings, booleans)
  returns the literal itself

`'var` is an abbreviated quoted expression, that will simply return var, without computing it.

`(name variable1 variable2 ...)`, The first item in this format is known as an Atomic type.

The the first variable name is an identifier, also called "atom" in the lisp context.
The subsequent variables, are not necessarily values, but themselves expressions, all of which will be evaluated before the function call.

This is important, as in order to pass lists as variables, we need to quote them, `'(1 2 3 4 5)`.
This is because once it's evaluated as mentioned before, in the evaluation of the function call, after parsing the expression, the recieved value is the list itself.

If input is simply `(1 2 3 4 5)`, the interpreter will throw error because it attempts to find a function with the name 1.
This is valid, behaviour by design. All list variables in functions, should be quoted.

## Special Forms
In Lisp there exists the concept of Special Forms, which are fundamental expressions, that are evaluated differently from the general function types. These are specially defined, as using these we can build bigger more fully featured functions and more complex structure. Special forms are used to describe variable assignment, control flow, function declarations and associated syntax.

The special forms are implemented here as specified for [MIT SCHEME](https://edoras.sdsu.edu/doc/mit-scheme-9.2/mit-scheme-ref/Special-Forms.html).

## Primitives
Just like how complex mathematical formulae require the existence of a set of fundamental theorems, which are assumed to be true.

Primitives are the minimal set of functions, which we can combine to create as complicated a function as we need to.

This can be considered as the fundamental instruction set, using which we create arbitrary programs.

In this project the Primitives are implemented in Haskell, and all further functions can be written in thkeme using the primitives.

## Running
```
stack run
```

This will launch a REPL in which we can type play around with arbitrary expressions and see what kind of values they return.

