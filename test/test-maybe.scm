(import (rnrs (6))
        (monads)
        (monads maybe))

(define (test-context)
  (define-context x (a 1) (b 2))
  (with-x
    (assert (= a 1))
    (assert (= b 2))))

(define (div x y)
  (if (= 0 y)
    *failure*
    (/ x y)))

(define (test-with-maybe)
  (define (inc x) (+ 1 x))
  (with-maybe
    (assert (failure? (>>= (div 1 0) inc)))
    (assert (= 3/2    (>>= (div 1 2) inc)))
    (assert (= 42     (return 42)))))

(define (test-seq-maybe)
  (define (f x)
    (seq-maybe
      (a <- (div 1 x))
      (b <- (+ a 1))
      (return b)))

  (assert (failure? (f 0)))
  (assert (= 4/3 (f 3))))
