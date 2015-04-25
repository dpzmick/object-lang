(module store (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "types.scm")

  (define the-store '())

  (define (init-store) (set! the-store '()))

  (define (refid r)
    (cases reference r
           (ref (id) id)))

  (define (new-store-replacing idx val)
    (letrec
        ([aux
          (lambda (i remain)
            (if (null? remain)
                '()
                (let
                    ([my-elem
                      (if (= i idx)
                          val
                          (car remain))])
                  (cons my-elem (aux (- i 1) (cdr remain))))))])
      (aux (- (length the-store) 1) the-store)))

  (define (edit-store r value)
    (cases reference r
           (ref (refid) (set! the-store (new-store-replacing refid value)))))

  (define (store-lookup refid)
    (letrec
        ([aux
          (lambda (idx remain)
            (if (= -1 idx)
                (null-val)
                (if (= idx refid)
                    (car remain)
                    (aux (- idx 1) (cdr remain)))))])
      (aux (- (length the-store) 1) the-store)))

  (define (recur-store-lookup r)
    (cases reference r
           (ref (refid) (cases expressed-val (store-lookup refid)
                               (ref-val (r) (recur-store-lookup r))
                               (else (store-lookup refid))))))

  (define (new-ref value)
    (let*
        ([ref-id (length the-store)]
         [new-store (cons value the-store)])
      (begin
        (set! the-store new-store)
        (ref ref-id))))

  )
