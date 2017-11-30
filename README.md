R6RS monads
===========

This package implements genereric syntax for working with monads in R6RS scheme.

Main syntax
-----------

Given a monad `m` with chain operator `m->>=` and return operator `m-return`, we
can define syntax:

```scheme
(define-monad m m->>= m-return)
```

This defines two new pieces of syntax: `with-m` and `seq-m`. The first bundles
the monadic operators in a context:

```scheme
(with-m
  (>>= value (lambda (x) (return (* x 42)))))
```

The `seq-m` operator provides a syntax very similar to Haskell's `do`.

```scheme
(seq-m
  (x <- (div 1 a))
  (y <- (+ x 1))
  (return y))
```

Example: making a parser
------------------------

In this example we show how to build a very simple parser using the parser (state)
monad in this libary. I learned how to build a monadic parser from Graham Hutton's
book "Programming in Haskell". This is also explained in [Function pearls - Monadic
parsing in Haskell](http://eprints.nottingham.ac.uk/223/1/pearl.pdf) by Graham Hutton
and Eric Meijer.

The `parser` monad is defined in `(monads parser)`.

```scheme
(import (rnrs (6))
        (monads)
        (monads parser))
```

A value in the parser monad (or just 'parser') is a function that takes a state
and returns `values` with a result and a new state.

In this case, our state variable is a list. We will take elements from this
list one by one. Our first parser is the function `element`. It takes an
element from a list and returns it along with the rest of the list.
If the list was empty, we return `*failure*`.

```scheme
(define (element c)
  (if (null? c)
    (values *failure* c)
    (values (car c) (cdr c))))
```

We can build a slightly more advanced parser, that filters the output of
`element`.

```scheme
(define (satisfies pred?)
  (seq-parser
    (x <- element)
    (if (pred? x)
      (return x)
      parser-failure)))
```

Note that, since the values in the parser monad are functions, `seq-parser`
returns a function similar to `element`: one that takes a state and returns
a value and a new state.

Using `satisfies` we can build a parser that accepts only values that are
`eq?` to a given value.

```scheme
(define (equals x)
  (satisfies (lambda (y) (eq? x y))))
```

Using these basic building blocks we can build a very simple parser that
reads a recursive list structure.

```scheme
(define number
  (satisfies number?))

(define list-of-items
  (seq-parser
    (x <- (equals '<))
    (y <- (many item))
    (z <- (equals '>))
    (return y)))

(define item
  (choice number list-of-items))
```

Running this on a small test:

```scheme
#| Test the code on a sample |#
(let ((input '(< 1 2 < 3 4 > < < 5 > 6 > 7 >)))
  (display (parse list-of-items input))
  (newline))
```

gives the output

```scheme
(1 2 (3 4) ((5) 6) 7)
```

### Running this example
This example can be found in the `examples` folder, and run with Guile

```bash
guile -L lib examples/parser.scm
```

or with Chez Scheme

```bash
scheme --libdirs lib --script examples/parser.scm
```
