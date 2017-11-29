(library (monads)

  (export define-monad define-context <-
          failure? make-failure failure-trace failure-exception *failure*
          with-maybe seq-maybe
          with-parser seq-parser parser-failure)

  (import (rnrs (6))
          (monads receive)
          (monads syntax))

  #| Failure object
   |#
  (define-record-type failure
    (fields exception trace)
    (protocol
      (lambda (new)
        (case-lambda
          (()    (new #f #f))
          ((e)   (new e #f))
          ((e t) (new e t))))))

  (define *failure* (make-failure))

  #| ----------------------------------------------------------------
   |  Maybe monad
   |#
  (define (maybe->>= value f)
    (if (failure? value)
      value
      (f value)))

  (define maybe-return values)

  (define-monad maybe maybe->>= maybe-return)

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
)
