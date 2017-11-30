(library (monads)

  (export
    ;;; syntax
    define-monad define-context <-

    ;;; failure
    failure? make-failure failure-trace failure-exception *failure*

    ;;; maybe
    with-maybe seq-maybe

    ;;; parser
    with-parser seq-parser parser-failure parse choice many some)

  (import (rnrs (6))
          (monads failure)
          (monads parser)
          (monads maybe)
          (monads syntax))
)
