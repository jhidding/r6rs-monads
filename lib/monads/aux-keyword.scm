#| Code snippet from Andy Keep |#
(library (monads aux-keyword)
  (export define-auxiliary-keyword
          define-auxiliary-keywords)

  (import (rnrs (6)))

  (define-syntax define-auxiliary-keyword
    (syntax-rules ()
      [(_ name)
       (define-syntax name
         (lambda (x)
           (syntax-violation #f "misplaced use of auxiliary keyword" x)))]))

  (define-syntax define-auxiliary-keywords
    (syntax-rules ()
      [(_ name* ...)
       (begin (define-auxiliary-keyword name*) ...)]))
)
