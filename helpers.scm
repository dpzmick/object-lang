#lang eopl

(define (fold f init lst)
  (letrec
    ([aux
       (lambda (acc remain)
         (if (null? remain)
             acc
             (aux (f acc (car remain)) (cdr remain))))])
       (aux init lst)))

(define (zip l1 l2)
  (letrec
    ([aux
          (lambda (top1 top2)
            (if (or (null? top1) (null? top2))
                '()
                (cons (cons (car top1) (car top2)) (aux (cdr top1) (cdr top2)))))])
    (aux l1 l2)))

  (define (search-list lst1 lst2 searchval)
    (if (null? lst1)
        '()
        (if (eq? (car lst1) searchval)
            (car lst2)
            (search-list (cdr lst1) (cdr lst2) searchval))))

(define (do-nothing v) '())
(trace do-nothing)

(provide (all-defined-out))
