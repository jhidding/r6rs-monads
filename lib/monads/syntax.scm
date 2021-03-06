(library (monads syntax)
  (export define-monad define-context <-)

  (import (rnrs (6))
          ; (only (chezscheme) trace-define-syntax)
          (monads gen-id)
          (monads aux-keyword))

  #| Defines a context; think of it as a persistent let-binding.
   |
   |   (define-context <name> (<var> <value>) ...)
   |
   | creates a new syntax `with-<name>` that expands to
   |
   |   (letrec ((<var> <value>) ...) <expression>)
   |
   | such that the variables <var> ... are in scope of <expression>.
   |#
  (define-syntax define-context
    (lambda (x)
      (syntax-case x ()
        ((define-context <name> (<var> <expr>) ...)
         (with-syntax ((<with> (gen-id #'<name> "with-" #'<name>)))
           #'(define-syntax <with>
               (lambda (x)
                 (syntax-case x ()
                   ((<with> <<expr>> (... ...))
                    (with-syntax ((<var> (datum->syntax #'<with> '<var>))
                                  ...)
                      #'(letrec ((<var> <expr>) ...)
                          <<expr>> (... ...)))))))
           )))))

  #| Defines a context for the defined monad. In addition to the given
   | parameters for the chain and return operators, the sequence syntax is
   | defined.
   |
   | For a generic monad, this syntax looks like:
   |
   |   (define-syntax seq
   |     (syntax-rules (<-)
   |       ;; last one get's out
   |       ((seq foo) foo)
   |
   |       ;; chain notation
   |       ((seq (arg <- foo) rest ...)
   |        (>>= foo (lambda (arg) (seq rest ...))))
   |
   |       ;; side-effects, result is not used
   |       ((seq foo rest ...)
   |        (>>= foo (lambda (_) (seq rest ...))))))
   |
   | We use the contexts defined above to create specific syntax for each
   | defined monad. This means that, within the `seq` syntax, the operators
   | `>>=` and `return` work as expected.
   |#
  (define-syntax define-monad
    (lambda (x)
      (syntax-case x ()
        ((define-monad <name> <chain-def> <return-def> (<extra-vars> <extra-defs>) ...)
         (with-syntax ((<with>   (gen-id #'<name> "with-" #'<name>))
                       (<seq>    (gen-id #'<name> "seq-" #'<name>)))
           #'(begin
               (define-context <name>
                 (>>=    <chain-def>)
                 (return <return-def>)
                 (<extra-vars> <extra-defs>) ...)

               (define-syntax <seq>
                 (lambda (y)
                   (define (wrap ctx . body)
                     (with-syntax ((<chain>  (datum->syntax ctx '>>=))
                                   (<return> (datum->syntax ctx 'return))
                                   (<extra-vars> (datum->syntax ctx '<extra-vars>))
                                    ...)
                       #`(letrec ((<chain>  <chain-def>)
                                  (<return> <return-def>)
                                  (<extra-vars> <extra-defs>) ...)
                           #,@body)))

                   (syntax-case y (<-)
                     ((<seq> <<f>>)
                      (wrap #'<seq> #'<<f>>))

                     ((<seq> (<<formals>> (... ...) <- <<f>>) <<rest>> (... ...))
                      (wrap #'<seq>
                            #'(<chain-def>
                                <<f>> (lambda (<<formals>> (... ...))
                                        (<seq> <<rest>> (... ...))))))

                     ((<seq> <<f>> <<rest>> (... ...))
                      (wrap #'<seq>
                            #'(<chain-def>
                                <<f>> (lambda _
                                        (<seq> <<rest>> (... ...))))))))))
             )))))

  (define-auxiliary-keyword <-)
)
