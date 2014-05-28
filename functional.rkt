(module access racket/base
  (require "prelude.rkt" "common.rkt")
  (provide filter-break remove-break)
  
  (define (filter-break f l)
    "Filter a list, but stop collecting when f is false."
    (if
     (or (null? l) (not (f (car l))))
     nil
     (cons (car l) (filter-break f (cdr l)))))

  (define (remove-break f l)
    "When f is false in a list, supply the rest of the list."
    (safely l
            (cond 
             ((or (null? l) (not (f (car l)))) l)
             (#t (remove-break f (cdr l))))
            nil))
)
  
