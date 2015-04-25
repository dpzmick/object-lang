(module express (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "mp5-parser.scm")
  (require "types.scm")
  (require "store.scm")

  (define (expressed-val->native-val val)
    (cases expressed-val val
           (int-val (value) value)
           (bool-val (value) value)
           (proc-val (value) 'undefined)
           (ref-val (r) (expressed-val->native-val (recur-store-lookup r)))
           (obj-val (o) 'undefined)
           (null-val () 'undefined)))

  (define (extract-value expressed)
    (cases expressed-val expressed
           (int-val (value) value)
           (bool-val (value) value)
           (proc-val (value) value)
           (ref-val (refid) refid)
           (obj-val (o) o)
           (null-val () 'undefined)))

  (define (wrap-it value)
    (cond
     [(number? value) (int-val value)]
     [(boolean? value) (bool-val value)]
     [(procedure? value) (proc-val value)]
     [(reference? value) (ref-val value)]
     [(object? value) (obj-val value)]
     [else (null-val)]))
  )
