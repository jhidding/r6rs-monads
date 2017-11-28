(library (monads syntax)
  (export define-monad define-context <-)

  (import (rnrs (6))
          (monads gen-id)
          (monads aux-keyword))
          ; (only (chezscheme) trace-define-syntax meta))

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
   | parameters for the chain and return operators, the sequence syntax
   | is defined.
   |#
  (define-syntax define-monad
    (lambda (x)
      (syntax-case x ()
        ((define-monad <name> <chain-def> <return-def>)
         (with-syntax ((<with>   (gen-id #'<name> "with-" #'<name>))
                       (<seq>    (gen-id #'<name> "seq-" #'<name>))
                       (<chain>  (datum->syntax #'<name> '>>=))
                       (<return> (datum->syntax #'<name> 'return)))
           #'(begin
               (define-context <name>
                 (<chain>  <chain-def>)
                 (<return> <return-def>))

               (define-syntax <seq>
                 (syntax-rules (<-)
                   ((_ <<f>>) <<f>>)
                   ((_ (<<formal>> <- <<f>>) <<rest>> (... ...))
                    (<with>
                      (<chain> <<f>>
                               (lambda (<<formal>>) (<seq> <<rest>> (... ...))))))
                   ((_ <<f>> <<rest>> (... ...))
                    (<with>
                      (<chain> <<f>> (lambda (x) (<seq> <<rest>> (... ...)))))))))
           )))))

  (define-auxiliary-keyword <-)
)
