(library (monads)

  (export
    ;;; syntax
    define-monad define-context <-

    ;;; failure
    failure? make-failure failure-trace failure-exception *failure*)

  (import (rnrs (6))
          (monads failure)
          (monads syntax))
)
