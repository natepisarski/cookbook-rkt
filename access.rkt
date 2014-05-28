(module access racket/base
  (provide count contains);qsort pull refpos areAll isBefore surrounds)
  (require "prelude.rkt" "common.rkt" "functional.rkt")
  
  (define (count a b)
    "Count the elements from a list."
    (apply + (map (lambda (x) (if (= x b) 1 0)) a)))
  
  (define (contains a b)
    "Find if a list contains a sub list or element."
    (if (list? b)
        (safely a
                (if (equal? (stop a (length b)) b) #t (contains (cdr a) b))
                #f)
        (elem a b)))
  
  (define (qsort a)
    "Sort a list of Ords."
    (safely a
            (safely (cdr a)
                    (let ([lesser (qsort (filter  (lambda (x) (>= (car a) x )) (cdr a)))]
                          [greater (qsort (filter (lambda (x) (<  (car a) x )) (cdr a)))])
                      (append lesser (list (car a)) greater))
                    (list (car a))) nil))
  
  (define (!! a b)
    "Pull implementation. Get an element at an index from the list."
    (safely a
     (if
      (= b 0)
      (car a)
      (!! (cdr a) (dec b)))
     (error "Exception in !!. Index not possible.")))
  
  (define (refpos a b c)
    "Referential positioning. When c is in a, get that index from b."
    (!! b (pos a c)))

  (define (areAll a b)
    "Are all elements of a list this element?"
    (= 
     (length (filter-break (lambda (x) (equal? x b)) a))
      (length a)))

  (define (isBefore a b)
    "Is b before a?"
    (equal? (stop a (length b)) b))
  
  (define (surrounds a b c)
    "Are b and c around a?"
    (and (equal? (car a)  b)
         (equal? (last a) b)))
     
 )
