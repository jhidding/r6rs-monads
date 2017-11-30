(import (rnrs (6))
        (monads)
        (monads parser))

#| Get an item from input list |#
(define (element c)
  (if (null? c)
    (values *failure* c)
    (values (car c) (cdr c))))

#| Get an item from input that satisfies `pred?` |#
(define (satisfies pred?)
  (seq-parser
    (x <- element)
    (if (pred? x)
      (return x)
      parser-failure)))

#| Get an item from input that equals `x` |#
(define (equals x)
  (satisfies (lambda (y) (eq? x y))))

#| Get a <number> |#
(define number
  (satisfies number?))

#| Get a <list>: many <item>s |#
(define list-of-items
  (seq-parser
    (x <- (equals '<))
    (y <- (many item))
    (z <- (equals '>))
    (return y)))

#| Get an <expr>: <number> or <list> |#
(define item
  (choice number list-of-items))

(let ((input '(< 1 2 < 3 4 > < < 5 > 6 > 7 >)))
  (display (parse list-of-items input))
  (newline))
