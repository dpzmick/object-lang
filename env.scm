(module env (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "helpers.scm")
  (require "types.scm")

  (define (get-env env var)
    (cases environment env
           (empty-env () (null-val))

           (rec-env (names funs other)
                    (let
                        ([found-val (search-list names funs var)])
                      (if (null? found-val)
                          (get-env other var)
                          (cases expressed-val found-val
                                 (proc-val (p) (cases proc p
                                                      (a-procedure (vars body c)
                                                                   (proc-val (a-procedure vars body env)))))
                                 (else (null-val))))))

           (extended-env (myvar myval other)
                         (if (eq? var myvar)
                             myval
                             (get-env other var)))))


  (define (extended-env-at-end var val e)
    (cases environment e
           (empty-env ()
                      (extended-env var val (empty-env)))

           (rec-env (names funs other)
                    (rec-env names funs (extended-env-at-end var val other)))

           (extended-env (a b other)
                         (extended-env a b (extended-env-at-end var val other)))))
  )
