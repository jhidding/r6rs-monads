(import (rnrs (6))
        (monads)
        (monads parser))


(define (item c)
  (if (null? c)
    (values *failure* c)
    (values (car c) (cdr c))))


(define (satisfies pred?)
  (seq-parser
    (x <- item)
    (if (pred? x)
      (return x)
      parser-failure)))


(define (test-parsers)
  (let ((p (some (satisfies number?))))
    (assert (failure? (parse p '(a b c))))
    (assert (equal? '(1 2 3) (parse p '(1 2 3)))))

  (let ((p (many (seq-parser
                   (x <- item)
                   (y <- item)
                   (return (cons y x))))))
    (assert (equal? '((b . a) (d . c) (f . e))
                    (parse p '(a b c d e f))))))


(define (test-parser-recursion)
  (define (equals x)
    (satisfies (lambda (y) (eq? x y))))

  (define number
    (satisfies number?))

  (define list-of-items
    (seq-parser
      (x <- (equals '<))
      (y <- (many item))
      (z <- (equals '>))
      (return y)))

  (define item
    (choice number list-of-items))

  (let ((input '(< 1 2 < 3 4 > < < 5 > 6 > 7 >)))
    (assert (equal? '(1 2 (3 4) ((5) 6) 7)
                    (parse list-of-items input)))))

