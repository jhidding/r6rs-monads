(import (rnrs (6))
        (monads)
        (monads receive))

(define (item c)
  (if (null? c)
    (values *failure* c)
    (values (car c) (cdr c))))

#| Tries to parse with p1, if that fails, take p2.
 |#
(define (choice2 p1 p2)
  (lambda (cur1)
    (receive (v cur2) (p1 cur1)
      (if (failure? v)
        (p2 cur1)
        (values v cur2)))))

#| Tries to parse with p, if that fails, parses with (choice ps)
 |#
(define (choice p . ps)
  (fold-left choice2 p ps))

#| Filters items using predicate `pred`
 |#
(define (sattisfies pred?)
  (seq-parser
    (x <- item)
    (if (pred? x)
      (return x)
      parser-failure)))

#| Accepts any number of parsings with `p`.
 |#
(define (many p)
  (with-parser
    (choice (some p) (return '()))))

#| Accepts one or more parsings with `p`.
 |#
(define (some p)
  (seq-parser
    (v <- p)
    (vs <- (many p))
    (return (cons v vs))))

(define (parse-result p input)
  (receive (result _) (p input)
    result))

(define (test-parsers)
  (let ((p (some (sattisfies number?))))
    (assert (failure? (parse-result p '(a b c))))
    (assert (equal? '(1 2 3) (parse-result p '(1 2 3)))))

  (let ((p (many (seq-parser
                   (x <- item)
                   (y <- item)
                   (return (cons y x))))))
    (assert (equal? '((b . a) (d . c) (f . e))
                    (parse-result p '(a b c d e f))))))

