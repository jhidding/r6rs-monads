(import (rnrs (6))
        (monads)
        (monads receive)
        (monads record-contexts)
        (monads parser))

#| Reader should mimic functional file IO |#
(define-record-context reader
  (fields text pos))

(define (reader-forward r)
  (with-reader r
    (make-reader text (+ 1 pos))))

(define (reader-peek r)
  (with-reader r
    (string-ref text pos)))

(define (reader-end? r)
  (with-reader r
    (>= pos (string-length text))))


#| Basic operations |#
(define (item r)
  (if (reader-end? r)
    (values *failure* r)
    (values (reader-peek r) (reader-forward r))))

(define (sat pred)
  (seq-parser
    (c <- item)
    (if (pred c)
      (return c)
      parser-failure)))

(define (is-char c)
  (sat (lambda (x) (char=? x c))))

(define (is-list lst)
  (if (null? lst)
    (seq-parser (return '()))
    (seq-parser
      (is-char (car lst))
      (is-list (cdr lst))
      (return lst))))

(define (is-string s)
  (is-list (string->list s)))

(define (sep-by p sep)
  (define (sep-by* p sep)
    (seq-parser
      (a  <- p)
      (as <- (many (seq-parser sep p)))
      (return (cons a as))))

  (choice sep-by* parser-failure))

(define (chainl* p op)
  (define (rest a)
    (choice (seq-parser
              (f <- op)
              (b <- p)
              (rest (f a b)))
            (with-parser (return a))))

  (seq-parser (a <- p) (rest a)))

(define (chainl p op a)
  (with-parser
    (choice (chainl1* p op) (return a))))

(define space (many (is-char #\space)))

(define (token p)
  (seq-parser
    (a <- p) space (return a)))

(define (symb cs)
  (token (is-string cs)))

(define (parse p r)
  (receive (result forget-r) ((seq-parser space p) r)
    result))

(define number 
  (seq-parser
    (x <- (token (some (sat char-numeric?))))
    (return (string->number (list->string x)))))

(define addop
  (choice
    (seq-parser
      (symb "+") (return +))
    (seq-parser
      (symb "-") (return -))))

(define mulop
  (choice
    (seq-parser
      (symb "*") (return *))
    (seq-parser
      (symb "/") (return /))))

(define (term) (chainl* (factor) mulop))
(define (expr) (chainl* (term) addop))
(define (factor) (choice number (seq-parser
                                (symb "(")
                                (n <- (expr))
                                (symb ")")
                                (return n))))

(display (parse (expr) (make-reader " 1 - 2 * 3 + 4" 0)))
(newline)

