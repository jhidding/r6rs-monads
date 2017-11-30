(library (monads parser)
  (export with-parser seq-parser parser-failure
          parse choice some many)

  (import (rnrs (6))
          (monads receive)
          (monads failure)
          (monads syntax))

  #| ----------------------------------------------------------------
   |  Parser monad
   |#
  (define (parser-return value)
    (lambda (cursor)
      (values value cursor)))

  #| Returns *failed*, doesn't consume.
   |#
  (define (parser-failure cursor)
    (values *failure* cursor))

  #| Chain operator
   |#
  (define (parser->>= parser f)
    (lambda (cursor)
      (receive (value cursor) (parser cursor)
        (if (failure? value)
          (values value cursor)
          ((f value) cursor)))))

  (define-monad parser parser->>= parser-return)

  #| Tries to parse with p1, if that fails, take p2.
   |#
  (define (choice2 p1 p2)
    (lambda (cur1)
      (receive (v cur2) (p1 cur1)
        (if (failure? v)
          (p2 cur1)
          (values v cur2)))))

  #| Tries to parse with p, if that fails, parses with (choice ps)
   |#
  (define (choice p . ps)
    (fold-left choice2 p ps))

  #| Accepts any number of parsings with `p`.
   |#
  (define (many p)
    (with-parser
      (choice (some p) (return '()))))

  #| Accepts one or more parsings with `p`.
   |#
  (define (some p)
    (seq-parser
      (v <- p)
      (vs <- (many p))
      (return (cons v vs))))

  #| Applies input to parser p, then returns only the result.
   |#
  (define (parse p input)
    (receive (result _) (p input)
       result))
  )
