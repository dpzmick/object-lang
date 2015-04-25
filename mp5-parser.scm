(module mp5-parser (lib "eopl.ss" "eopl")

  (provide (all-defined-out))

  (define the-lexical-spec
    '(
      (Whitespace
       (whitespace)
       skip)

      (id
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)

      (int
       (digit (arbno digit))
       number)

      (int
       ("-" digit (arbno digit))
       number)
      ))

  (define the-grammar
    '(
      (program
       (expr)
       a-program)

      (expr
       (aop "(" expr (arbno "," expr) ")")
       arith-expr)

      (expr
       (cop "(" expr "," expr ")")
       comp-expr)

      (expr
       ("proc" "(" (arbno id) ")" expr "end")
       proc-expr)

      (expr
       ("(set" expr expr ")")
       set-expr)

      (expr
       (int)
       const-expr)

      (expr
       ("true")
       true-expr)

      (expr
       ("false")
       false-expr)

      (expr
       (object-expr (arbno "." id))
       message-expr)

                                        ; ---

      (aop ("+") add-op)
      (aop ("-") sub-op)
      (aop ("*") mul-op)
      (aop ("/") div-op)

      (cop ("<") lt-op)
      (cop (">") gt-op)
      (cop ("=") eq-op)

                                        ; ---

      (object-expr
       ("begin" (arbno expr ";")  "end")
       begin-expr)

      (object-expr
       ("if" expr "then" expr "else" expr "end")
       if-expr)

      (object-expr
       ("let" (arbno id "=" expr) "in" expr "end")
       let-expr)

      (object-expr
       ("letrec" (arbno id "=" expr) "in" expr "end")
       letrec-expr)

      (object-expr
       ("(" expr (arbno expr) ")")
       app-expr)

      (object-expr
       (id)
       id-expr)

      (object-expr
       ("self")
       self-expr)

      (object-expr
       ("super")
       super-expr)

      (object-expr
       ("EmptyObj")
       empty-obj-expr)

      (object-expr
       ("extend" expr "with" (arbno member-decl))
       extend-expr)

                                        ; ---
      (member-decl
       ("public" id "=" expr ";")
       public-decl-expr)

      (member-decl
       ("protected" id "=" expr ";")
       protected-decl-expr)))

  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  (define parser (sllgen:make-string-parser the-lexical-spec the-grammar))
 )
