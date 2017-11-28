(library (monads)

  (export define-monad define-context <-
          with-maybe seq-maybe failure? make-failure
          failure-trace failure-exception)

  (import (rnrs (6))
          (monads syntax))

  (define-record-type failure
    (fields exception trace)
    (protocol
      (lambda (new)
        (case-lambda
          (()    (new #f #f))
          ((e t) (new e t))))))

  (define (maybe->>= value f)
    (if (failure? value)
      value
      (f value)))

  (define maybe-return values)

  (define-monad maybe maybe->>= maybe-return)
)
