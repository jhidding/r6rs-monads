(library (monads gen-id)
  (export gen-id)
  (import (rnrs (6)))

  ; Define a new symbol, code from TSPL Chapter 8.
  (define gen-id
    (lambda (template-id . args)
      (datum->syntax template-id
        (string->symbol
          (apply string-append
            (map (lambda (x)
                   (if (string? x)
                       x
                       (symbol->string (syntax->datum x))))
                 args))))))
)
