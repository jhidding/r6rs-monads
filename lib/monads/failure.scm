(library (monads failure)
  (export failure? make-failure failure-exception failure-trace *failure*)
  (import (rnrs (6)))

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
)
