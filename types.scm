(module types (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "mp5-parser.scm")

  (define-datatype environment environment?
    (empty-env)

    (rec-env
     (names (list-of symbol?))
     (funs (list-of expressed-val?))
     (other environment?))

    (extended-env
     (var symbol?)
     (val expressed-val?)
     (other environment?)))

  (define-datatype expressed-val expressed-val?
    (int-val (value number?))
    (bool-val (value boolean?))
    (proc-val (value proc?))
    (ref-val (r reference?))
    (obj-val (o object?))
    (null-val))

  (define-datatype proc proc?
    (a-procedure
     (bindings (list-of symbol?))
     (body expr?)
     (env environment?)))

  (define-datatype reference reference?
    (ref (refid number?)))

  ; object stuff
  (define-datatype access-modifier access-modifier?
    (public)
    (protected))
  
  (define-datatype field field?
    (a-field
     (field-name symbol?)
     (visibility access-modifier?)))

  (define-datatype object object?
    (empty-obj)
    (an-object
     (my-class number?)
     (super-obj object?)
     (fields (list-of field?))
     (refs (list-of reference?))))
  )
