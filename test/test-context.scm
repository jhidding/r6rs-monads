(import (rnrs (6))
        (monads))

(define-context x (a 1) (b 2))

(define *failure* (make-failure))

(define (div x y)
  (if (= 0 y)
    *failure*
    (/ x y)))

(with-maybe
  (display (>>= (div 1 0) (lambda (x) (+ x 1)))) (newline)
  (display (>>= (div 1 2) (lambda (x) (+ x 1)))) (newline))

(display
  (with-maybe (return 42))) (newline)

(display (seq-maybe
  (x <- (div 1 0))
  (y <- (+ x 1))
  (return y))) (newline)

(display (with-maybe (seq-maybe
  (x <- (div 1 3))
  (y <- (+ x 1))
  (z <- (return y))
  z))) (newline)

(with-x
  (display (+ a b)))
(newline)
