(module object (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "types.scm")
  (require "store.scm")
  (require "helpers.scm")

  (define the-counter 0)
  (define (next-class-counter) (begin (set! the-counter (+ 1 the-counter)) the-counter))

  (define (is-empty-obj? ob)
    (cases object ob
           (empty-obj () #t)
           (else #f)))

  (define (field-name f)
    (cases field f
           (a-field (name vis) name)))

  (define (get-class o)
    (cases object o
           (empty-obj () -1)
           (an-object (class s f r) class)))

  (define (field-visibility f)
    (cases field f
           (a-field (name vis) vis)))

  (define (new-obj-from o)
    (cases object o
           (empty-obj () (empty-obj))
           (an-object (class super fields refs)
                      (let
                          ([new-super (new-obj-from super)]
                           [new-refs (map (lambda (e) (new-ref (store-lookup (refid e)))) refs)])
                        (an-object class new-super fields new-refs)))))

  (define (is-in-tree ob1 ob2)
    (if (is-empty-obj? ob2)
        #f
        (cases object ob1
               (empty-obj () #f)
               (an-object (classnum super f r)
                          (if (eq? (get-class ob2) classnum)
                              #t
                              (is-in-tree super ob2))))))

  (define (get-object-field obj name self)
    (cases object obj
           (empty-obj () (null-val))
           (an-object (c super fields refs)
                      (let
                          ([found (search-list (map field-name fields) refs name)])
                        (if (null? found)
                            (get-object-field super name self)
                            (if (is-in-tree obj self)
                                (ref-val found)
                                (let*
                                    ([visibility (map field-visibility fields)]
                                     [names (map field-name fields)]
                                     [found2 (search-list names visibility name)])
                                  (cases access-modifier found2
                                         (public () (ref-val found))
                                         (protected () (null-val))))))))))
  )
