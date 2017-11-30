(library (monads maybe)
  (export with-maybe seq-maybe)
  (import (rnrs (6))
          (monads syntax)
          (monads failure))

  (define (maybe->>= value f)
    (if (failure? value)
      value
      (f value)))

  (define maybe-return values)

  (define-monad maybe maybe->>= maybe-return)
)
