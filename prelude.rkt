(module prelude racket/base
  (provide zip ++ rev dec inc range /= nil elem not-elem last <~>)
  
  (define (zip a b)
    "Combine two lists into pairs"
    (if (or (null? a) (null? b))
        '()
        (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))
  
  (define (++ a b)
    "Put a single item in the last part of a list"
    (if (null? a)
        (cons b '())
        (cons (car a) (++ (cdr a) b))))
  
        
  (define (rev a) 
    "Reverse a list"
    (if (null? a) '()  (append (rev (cdr a))  (list (car a)))))
  
  (define (dec a) (- a 1))
  (define (inc a) (+ a 1))
  
  (define (range a [b 0])
    "Generate a range"
    (if (= b a) (list a) (cons a (range (inc a) b))))
  
  (define (/= a b) (not (equal? a b)))
  (define nil '())
  
  (define (elem a b)
    (if (null? a) #f
        (if (equal? (car a) b) #t
            (elem (cdr a) b))))
  
  (define (not-elem a b) (not (elem a b)))
  
  (define (last a)
    (if (null? (cdr a)) (car a) (last (cdr a))))
  
  (define-syntax-rule (<~> a b c) (b c a))
)
