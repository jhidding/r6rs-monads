(library (monads)

  (export define-monad define-context <-
          failure? make-failure failure-trace failure-exception *failure*
          with-maybe seq-maybe)

  (import (rnrs (6))
          (monads receive)
          (monads failure)
          (monads parser)
          (monads syntax))

  #| ----------------------------------------------------------------
   |  Maybe monad
   |#
  (define (maybe->>= value f)
    (if (failure? value)
      value
      (f value)))

  (define maybe-return values)

  (define-monad maybe maybe->>= maybe-return)

)
