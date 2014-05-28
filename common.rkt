(module common racket/base
  (provide safely start stop positions pos apply-all flt from-last intersperse)
  (require "prelude.rkt")
  
  (define-syntax-rule (safely a b c)
    (if (null? a) c b))
  
  (define (start a b)
    "Start a list at an index."
    (if (equal? b 0)
        a
        (safely a 
                (start (cdr a) (dec b))
                nil)))
  
  (define (stop a b)
    "Stop a list at an index, starting at zero."
    (if (<= b 0)
        '()
        (safely a
                (cons (car a) (stop (cdr a) (dec b)))
                '())))
  
  (define (positions a b)
    "Find the positions of the second argument in a."
    (filter (lambda (d) (/= d nil))
            (map (lambda (c)    (if (equal?   (car c) b) (cadr c) nil))
                    (zip a (range 0 (length a))))))
  
  (define (pos a b) (car (positions a b)))
  
  (define (apply-all a b)
    "Pass function arguments down a ladder of functions."
    (safely b 
            (apply-all ((car b) a)  (cdr b)) ;BEGIN is progn equivelant. displayln exists.
            a))
  
  (define (flt a)
    "Flatten a list one level."
    (safely a
            (append (car a) (flt (cdr a)))
            nil))
  
  (define (from-last l f) 
    "Perform a function form the last to the first element of the list."
    (rev (f (rev l))))
  
  (define (intersperse h k) 
    "Interpserse a list with elements."
    (safely h
            (append (list (car h) k) (intersperse (cdr h) k))
            nil))
  
)

