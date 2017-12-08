(library (monads failure)
  (export failure? make-failure failure-exception failure-trace *failure*)
  (import (rnrs (6)))

  #| Failure object
   |#
  (define-record-type failure
    (fields exception trace state)
    (protocol
      (lambda (new)
        (case-lambda
          (()      (new #f #f #f))
          ((e)     (new e #f #f))
          ((e t)   (new e t #f))
          ((e t s) (new e t s))))))

  (define *failure* (make-failure))
)
