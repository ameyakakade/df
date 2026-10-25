# DF

DF is a pure, dynamically typed, functional dialect of Lisp.
It is also a toy programming language.

## Why ?
This project was built for [Syntax Error 2026](https://hackathon.sdslabs.co/), under 30 hrs. As a
challenge, no AI tools or LLMs were used to write code, documentation
or for general help.

Minimal dependencies are used, I used custom parser combinators taken
from [another project of mine.](https://github.com/ameyakakade/baskell/blob/ea5324dbf6f3f8c315735183a5b125b220e0731d/src/Parser.hs)

## How to Build ?
DF is written in Haskell, you will need the *ghc compiler* and 
*cabal package manager*. Download these from https://www.haskell.org/

Navigate to this directory and run `cabal build`.
Use `cabal run interpreter -- [arguments]` to run the interpreter.

### Arguments
Arguments provided to the interpreter are parsed in the following way:
Everything not prefixed with '-' is treated as a file name. The order
of files matters. The file extension, does not.

`-ast` displays the parsed ast.
`-symbols` displays the globally defined symbols for the current program.
`-debug` displays a trace of lambda reductions (this is mostly unreadable).

## Examples
There are a few examples in [examples](https://github.com/ameyakakade/df/tree/main/examples) directory. They contain things like
defining `foldl` and `foldr`, defining and using `map`, and a
factorial function using recursion.

## How to Use?
This is a semi-tutorial on how to use this language. This will be
trivial if you are familiar with Lisp.

DF has all the primitives to make the usual FP constructs, i.e Maps,
Folds, etc.

### Datatypes, Defines and Quotes
The datatypes available are integers, symbols and lists.
A symbol is a name that when evaluated returns the list its bound
to. However, symbols can exist without a list being bound to them.

A list is made up of *atoms*. An atom is a symbol, number or list
seperated by spaces. For example, the list below has 4 atoms.
A list is also a atom, hence a list can contain other lists.
```racket
(add 2 3) ; A list
(add 2 3 (negate 4)) ; Also a list
```

*define* is used to bind symbols to atoms (or lists).
It also evaluates the expression before binding it.
```racket
(define a 13) 
(define b (+ 4 2)) 
```
`a` is bound to the number `13`.
`b` is bound to the number `6`.

A list when evaluated is treated as function application. This can be
avoided by using the `'` (quote) operator.
```racket
(add 1 3) ;; 1 and 3 are applied to add function.
(1 2 3)   ;; Error: trying to evaluate '1' as a function
'(1 2 3)  ;; Returns a list with 3 elements.
```

This operator is also used when bindings lambda's to a symbol.
```racket
(define inc '(lambda ('a) (+ a 1)))
```
Here, the lambda list is not evaluated by the define, similarly `a` is
not evaluated and is treated as argument name.

Using the quote symbol can be thought of as 'delaying the evaluation'
of a list.

### Functions
All functions are lambdas. Named functions are symbols bound to a list
representing a lambda.

The basic syntax of a lambda is:
```racket
(lambda (argument-list) (expr))
```

When evaluating a lambda, first the argument list is evaluated to get
the symbols, then the arguments provided are bound to the symbols in
the arg-list, then the expression given is evaluated.

This is why quote was used for a lone symbol. You could instead have a
expression that returns a symbol name.

```racket
; Creating lambdas and immediately applying them.
((lambda ('a) (+ a 1)) 68)
((lambda ('a 'b) (+ a b)) 68 1)
```

### Let blocks
These are a way to define scoped variables.
The basic syntax of a let block is:
```racket
(let ((expr1) (expr2) ...) expr)
```
The value of the last result will be returned.
```racket
(let
    ( (define a 32) )
    (+ 43 a)
  )
```

As expected, lambdas can also be bound in let blocks.
DF has variable shadowing, so you can redefine a variable which will
be restored to its previous value when the scope ends.
```racket
(define a 64)

(define res (let ( ( define a 32) )
  (let ( ( define a 31) )
    a
    )))
res
```
Value of `res` will be 31.

```racket
(define res (let ( ( define a 100) )
  (+ (let ( ( define a 31) ) a) a)
  )
 )
res
```
Value of `res` is 131.

### If block
The basic syntax of a if block is:
```racket
(if (condition-expr) (true-expr) (false-expr) )
;; OR
(if (condition-expr) (true-expr) )
```
It is obvious how the expressions are evaluated.
When the false expression is omitted, it is replaced with empty list.

*if* constructs checks if the condition-expression is equal to ()
(empty list or nil), if it is false expression is evaluated and returned.
Anything except '() (number, symbol, list) is considered true.

### Predefined functions
There are certain functions predefined in DF.

Among these are basic binary operands,
```racket
(+ 33 34)   ;; 67 !
(- 33 34)   ;; -1
(* 33 34)   ;; 1122
(/ 33 34)   ;; 0
(negate 45) ;; -45
```

Equality operator,
```racket
(= 33 34) ;; returns '()
(= 34 34) ;; returns (t)
```

And functions for creating and deconstructing lists.

*car* is used to get the first element of the list.
*cdr* is used to get the rest of the elements of the list.
```racket
(car '(1 2 3)) ;; returns 1
(cdr '(1 2 3)) ;; returns '(2 3)
```

*cons* is used to construct a list
```racket
(cons 1 2)
```

Meaning of these is simplified here, read more about cons cell here:
https://en.wikipedia.org/wiki/Cons

### Output
This language does not have side effects, but the value of the last
expression is displayed after evaluation.

There is a special symbol evaluated for output named `output`.
```racket
(define output '(hello world))
(+ 1 2)
```

Output:
```racket
cabal run interpreter -- demo.df

Completed evaluation successfully!

Results: Num 3
Output: List [Str "hello",Str "world"]
```

### Comments
Comments start with `;` and end with newline.
```racket
(+ 1 1) ; this is a comment
```
