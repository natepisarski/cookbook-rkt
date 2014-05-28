(module continuous racket/base
  (provide before after);after remove splice between after-atomic)
  (require "prelude.rkt" "common.rkt" "access.rkt" "functional.rkt")            
  
  (define (before a b)
    "Returns the part of a list before a list or element."
      (define (before-atomic a b)
        (stop a (pos a b)))

      (define (before-multiple a b)
        (cond
         ((not (contains a b)) nil)
         ((equal? (stop a (length b)) b) nil)
         (#t (cons (car a) (before-multiple (cdr a) b)))))

      (if (list? b) 
          (before-multiple a b) 
          (before-atomic a b)))
  
  
  (define (after a b)
    "Returns the part of a list after an element or a list."
    (define (after-atomic a b)
      (if (<~> b not-elem a)
          '()
          (cdr (remove-break (lambda (x) (not (equal? x b))) a))))
  
    (define (after-multiple a b)
      (cond
       ((null? a) nil)
       ((not (contains a b)) nil)
       ((equal? (stop a (length b)) b) (start a (length b)))
       (#t (after-multiple (cdr a) b))))

    (if (list? b) (after-multiple a b) (after-atomic a b)))
 
  
  (define (remove a b)
    "Remove an element or sub-list from a list."
    (define (remove-atomic a b)
      (filter (lambda (x) (/= x b)) a))
    
    (define (remove-multiple a b)
      (cond
       ((null? a) nil)
       ((equal? (stop a (length b)) b) (remove-multiple (start a (length b)) b))
       (#t (cons (car a) (remove-multiple (cdr a) b)))))

    (if (list? b) (remove-multiple a b) (remove-atomic a b)))
  
  (define (between a b c)
    "Get the sub-list between two elements."
    (before (after a b) c))
  
  (define (splice a b c)
    "Remove everything between two parts."
    (remove (remove (remove a (between a b c)) c) b))
)
