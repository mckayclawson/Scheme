;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1A Question5

;filter-by : Predicate List -> List
(define (filter-by p L)
  (if (null? L)
      '()
      (if (p (car L))
          (cons (car L) (filter-by p (cdr L)))
          (filter-by p (cdr L)))))

;(equal? (filter-by number? '(1 2 3 4 5 a b c d e '(1 2 3 4 5))) '(1 2 3 4 5))
;(equal? (filter-by number? '(() a 5 (1 2 3) (x y z) b 10)) '(5 10))
;(equal? (filter-by symbol? '(() a 5 (1 2 3) (x y z) b 10)) '(a b))

;find-less : NatNum List -> List
(define (find-less n nums) 
  (filter-by (lambda (x) (< x n)) nums))

;find-same : NatNum List -> List
(define (find-same n nums) 
  (filter-by (lambda (x) (= x n)) nums))

;find-more : NatNum List -> List
(define (find-more n nums) 
  (filter-by (lambda (x) (> x n)) nums))

;(equal? (find-less 3 '()) '())
;(equal? (find-same 3 '()) '())
;(equal? (find-more 3 '()) '())
;(equal? (find-less 3 '(1 2 3 4 5)) '(1 2))
;(equal? (find-same 3 '(1 2 3 4 5)) '(3))
;(equal? (find-more 3 '(1 2 3 4 5)) '(4 5))

