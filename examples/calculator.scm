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


#| Read a character |#
(define (item r)
  (if (reader-end? r)
    (values *failure* r)
    (values (reader-peek r) (reader-forward r))))

#| Read a character that satisfies pred |#
(define (sat pred)
  (seq-parser
    (c <- item)
    (if (pred c)
      (return c)
      parser-failure)))

#| Read a character equal to `c` |#
(define (is-char c)
  (sat (lambda (x) (char=? x c))))

#| Read a sequence of characters identical to those in `lst` |#
(define (is-list lst)
  (if (null? lst)
    (seq-parser (return '()))
    (seq-parser
      (is-char (car lst))
      (is-list (cdr lst))
      (return lst))))

#| Read a string identical to `s` |#
(define (is-string s)
  (seq-parser
    (u <- (is-list (string->list s)))
    (return (list->string u))))

#| Read a sequence of items `p` separated by `sep`. |#
(define (sep-by p sep)
  (define (sep-by* p sep)
    (seq-parser
      (a  <- p)
      (as <- (many (seq-parser sep p)))
      (return (cons a as))))

  (choice sep-by* parser-failure))

#| Read `p`, then read `op`, `p`, fold-left on the result
 | of applying the result of `op`.
 |#
(define chain-left
  (case-lambda
    ((value operator)
     (define (rest a)
       (choice (seq-parser
                 (f <- operator)
                 (b <- value)
                 (rest (f a b)))
               (with-parser (return a))))
     (seq-parser (a <- value) (rest a)))

    ((value operator alternative)
     (with-parser
       (choice (chain-left value operator)
               (return alternative))))))

#| Read many spaces |#
(define space (many (is-char #\space)))

#| Tokenize parser |#
(define (token p)
  (seq-parser
    (a <- p) space (return a)))

#| Read a tokenized string |#
(define (symb cs)
  (token (is-string cs)))

#| Apply parser, return only result |#
(define (parse p r)
  (receive (result forget-r) ((seq-parser space p) r)
    result))

#| Parse an integer |#
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

#| For these last three definitions, order of definition matters.
 | The original in Haskell is lazy.
 |#
(define factor
  (choice number
          (seq-parser
            (symb "(")
            (n <- expr)
            (symb ")")
            (return n))))

(define term (chain-left factor mulop))

(define expr (chain-left term addop))

(define (calculate s)
  (display s) (display " = ")
  (display (parse expr (make-reader s 0)))
  (newline))

(calculate " 1 - 2 * 3 + 4")
(calculate "3 * 42 / (6*7 - 3)")
