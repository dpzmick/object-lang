#lang eopl
(require "mp5-parser.scm")
(require "helpers.scm")
(require "store.scm")
(require "expressed.scm")
(require "env.scm")
(require "types.scm")
(require "object.scm")

(define (value-of-arith-expr op e es env)
  (let*
      ([all-terms (cons e es)]

       [all-terms-evaled (map (lambda (e) (value-of-expr e env)) all-terms)]

       [all-int (fold
		 (lambda (acc el)
		   (cases expressed-val el
			  (int-val (v) (and acc #t))
			  (else #f)))
		 #t
		 all-terms-evaled)])

    (if (not all-int)
        (null-val)
        (let
	    ([all-unboxed (map expressed-val->native-val all-terms-evaled)])
          (cases aop op
                 (add-op () (int-val (apply + all-unboxed)))
                 (sub-op () (int-val (apply - all-unboxed)))
                 (mul-op () (int-val (apply * all-unboxed)))
                 (div-op () (int-val (apply / all-unboxed))))))))

(define (my-lt v1 v2)
  (if (and (number? v1) (number? v2))
      (bool-val (< v1 v2))
      (null-val)))

(define (my-eq v1 v2)
  (if (and (number? v1) (number? v2))
      (bool-val (= v1 v2))
      (if (and (boolean? v1) (boolean? v2))
	  (bool-val (eq? v1 v2))
	  (null-val))))

(define (my-gt v1 v2)
  (if (and (number? v1) (number? v2))
      (bool-val (> v1 v2))
      (null-val)))

(define (value-of-comp-expr op e1 e2 env)
  (let
    ([value1 (expressed-val->native-val (value-of-expr e1 env))]
     [value2 (expressed-val->native-val (value-of-expr e2 env))])
    (cases cop op
           (lt-op () (my-lt value1 value2))
           (eq-op () (my-eq value1 value2))
           (gt-op () (my-gt value1 value2)))))

(define (value-of-begin-expr exprs env)
  (let*
      ([folder
	(lambda (acc e) (value-of-expr e env))]
       [val (fold folder 0 exprs)])
       val))

(define (value-of-if-expr condition t f env)
  (let
    ([value-cond (expressed-val->native-val (value-of-expr condition env))])

    (if (boolean? value-cond)
        (if value-cond
            (value-of-expr t env)
            (value-of-expr f env))
        (null-val))))

(define (value-of-let-expr vars exprs body env)
  (let*
     ([zipped (zip vars exprs)]
      [folder
       (lambda (acc e)
         (extended-env (car e) (value-of-expr (cdr e) acc) acc))]
      [new-env (fold folder env zipped)])
    (value-of-expr body new-env)))

(define (value-of-letrec-expr vars exprs body env)
  (let*
      ([evaled (map (lambda (e)
                      (cases expr e
                             (proc-expr (vars body) (proc-val (a-procedure vars body env)))
                             (else (null-val)))) exprs)]
       [new-env (rec-env vars evaled env)])
    (value-of-expr body new-env)))

(define (value-of-app-expr e1 args env)
  (letrec
      ([lhs (value-of-expr e1 env)]
       [eval-fun
	(lambda (vars body closure)
	  (let*

	      ([helper (lambda (e)
			 (let
			     ([v (value-of-expr e env)])
			   (cases expressed-val (value-of-expr e env)
                                  (ref-val (r) (recur-store-lookup r))
				  (else v))))]
	       [all-evaled-args (map helper args)]

	       [zipped (zip vars all-evaled-args)]
	       [folder (lambda (acc e)
			 (extended-env (car e) (cdr e) acc))]
	       [new-env (fold folder closure zipped)])

            (cases expressed-val (get-env env '%self)
                   (null-val () (value-of-expr body new-env))
                   (else (value-of-expr body (extended-env '%self (get-env env '%self) new-env))))))]

       [helper (lambda (e)
		 (cases expressed-val e
                        (ref-val (r) (helper (recur-store-lookup r)))
                        (proc-val (p) (cases proc p
                                             (a-procedure (vars body env) (eval-fun vars body env))))
			(else (null-val))))])
    (helper lhs)))

(define (value-of-super-expr env)
  (let
      ([self (get-env env '%self)])
    (cases expressed-val self
           (obj-val (o)
                    (cases object o
                           (empty-obj () (obj-val empty-obj))
                           (an-object (class super f r) (obj-val super))))
           (else (null-val)))))

(define (value-of-member-decl-expr decl env)
  (cases member-decl decl
         (public-decl-expr (name value) (cons (a-field name (public)) (value-of-expr value env)))
         (protected-decl-expr (name value) (cons (a-field name (protected)) (value-of-expr value env)))))

(define (value-of-extend-expr super-expr extensions env)
  (let*
      ([super (extract-value (value-of-expr super-expr env))]
       [new-super (new-obj-from super)]
       [new-fields-and-defs (map (lambda (e) (value-of-member-decl-expr e env)) extensions)]
       [new-fields (map car new-fields-and-defs)]
       [new-defs (map cdr new-fields-and-defs)]
       [new-refs (map (lambda (e) (new-ref e)) new-defs)])
    (obj-val (an-object (next-class-counter) new-super new-fields new-refs))))

(define (value-of-self-expr env)
  (get-env env '%self))

(define (value-of-obj-expr objexpr env)
  (cases object-expr objexpr
	 (begin-expr (ex) (value-of-begin-expr ex env))
	 (if-expr (condition true false) (value-of-if-expr condition true false env))
	 (let-expr (ids exprs body) (value-of-let-expr ids exprs body env))
	 (letrec-expr (ids exprs body) (value-of-letrec-expr ids exprs body env))
	 (app-expr (rhs args) (value-of-app-expr rhs args env))
	 (id-expr (id) (get-env env id))
	 (self-expr () (value-of-self-expr env))
	 (super-expr () (value-of-super-expr env))
	 (empty-obj-expr () (obj-val (empty-obj)))
	 (extend-expr (e1 withs) (value-of-extend-expr e1 withs env))))

(define (curr-self env)
  (cases expressed-val (value-of-self-expr env)
         (obj-val (o) o)
         (else (empty-obj))))

(define (value-of-message-expr objexpr calls env)
  (let*
      ([last-seen (value-of-obj-expr objexpr env)]
       [folder (lambda (acc el) (begin
                                  (set! last-seen acc)
                                  (cases expressed-val acc
                                         (obj-val (o) (get-object-field o el (curr-self env)))
                                         (ref-val (r)
                                                  (cases expressed-val (recur-store-lookup r)
                                                         (obj-val (o) (get-object-field o el (curr-self env)))
                                                         (else (null-val))))
                                       (else (null-val)))))]
       [res (fold folder last-seen calls)]
       [what-it-is (cases expressed-val res
                          (ref-val (r) (recur-store-lookup r))
                          (else res))])
    (if (and (reference? (extract-value res)) (proc? (extract-value what-it-is)))
        (let
            ([p (extract-value what-it-is)])
         (cases proc p
                 (a-procedure (bindings body closure)
                              (let*
                                  ([new-env (extended-env '%self last-seen closure)]
                                   [p (proc-val (a-procedure bindings body new-env))])
                                (cases expressed-val (get-env env '%self)
                                       (null-val ()
                                                 (begin
                                                   (edit-store (extract-value res) p)
                                                   ;; (do-nothing p)
                                                   res))
                                       (else res))))))
        res)))

(define (value-of-set-expr e1 e2 env)
  (let
      ([rhs (value-of-expr e1 env)]
       [lhs (value-of-expr e2 env)])
    (cases expressed-val rhs
           (ref-val (r) (begin (edit-store r lhs) (null-val)))
           (else (null-val)))))

(define (value-of-expr e env)
  (cases expr e
	 (arith-expr (op e1 es) (value-of-arith-expr op e1 es env))
	 (comp-expr (op e1 es) (value-of-comp-expr op e1 es env))
	 (proc-expr (bindings body) (proc-val (a-procedure bindings body env)))
	 (set-expr (e1 e2) (value-of-set-expr e1 e2 env))
	 (const-expr (value) (int-val value))
	 (true-expr () (bool-val #t))
	 (false-expr () (bool-val #f))
	 (message-expr (oexpr calls) (value-of-message-expr oexpr calls env))))

(define (value-of-program prog)
  (cases program prog
	 (a-program (e) (value-of-expr e (empty-env)))))

(define (printable e)
  (cases expressed-val e
         (int-val (v) v)
         (bool-val (v) (if v 'true 'false))
         (ref-val (r) (printable (recur-store-lookup r)))
         (else 'undefined)))

(define (object-interpreter str)
  (begin
    (init-store)
    ;; (value-of-program (parser str))))
    (printable (value-of-program (parser str)))))
