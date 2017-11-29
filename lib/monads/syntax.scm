(library (monads syntax)
  (export define-monad define-context <-)

  (import (rnrs (6))
          (monads gen-id)
          (monads aux-keyword)
          (only (chezscheme) trace-define-syntax meta))

  (define (context-transformer t-id vars exprs . body)
    (let ((bindings (map (lambda (v e)
                           (list (datum->syntax t-id (syntax->datum v)) e))
                           vars exprs)))
      bindings))

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
               (lambda (y)
                 (syntax-case y ()
                   ((<with> <<expr>> (... ...))
                    #`(letrec #,(context-transformer
                                 #'<with>
                                 (list #'<var> ...)
                                 (list #'<expr> ...))
                        <<expr>> (... ...))))))
           )))))

  (define-syntax define-context-old
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
                 (lambda (y)
                   (syntax-case y (<-)
                     ((<seq> <<f>>)
                      (with-syntax ((<>>=>    (datum->syntax #'<seq> '>>=))
                                    (<return> (datum->syntax #'<seq> 'return)))
                        #'(let ((<>>=>    <chain-def>)
                                (<return> <return-def>))
                          <<f>>)))

                     ((<seq> (<<formal>> <- <<f>>) <<rest>> (... ...))
                      (with-syntax ((<>>=>    (datum->syntax #'<seq> '>>=))
                                    (<return> (datum->syntax #'<seq> 'return)))
                        #'(let ((<>>=>    <chain-def>)
                                (<return> <return-def>))
                            (<>>=> <<f>> (lambda (<<formal>>) (<seq> <<rest>> (... ...)))))))

                     ((<seq> <<f>> <<rest>> (... ...))
                      (with-syntax ((<>>=>    (datum->syntax #'<seq> '>>=))
                                    (<return> (datum->syntax #'<seq> 'return)))
                        #'(let ((<>>=>    <chain-def>)
                                (<return> <return-def>))
                            (<>>=> <<f>> (lambda (x) (<seq> <<rest>> (... ...)))))))))))
             )))))

  (define-auxiliary-keyword <-)
)
