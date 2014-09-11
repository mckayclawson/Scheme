;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1A Question3

;insert : NatNum List -> List
(define (insert n nums)
  (if (null? nums)
      (list n)
      (if (<= n (car nums))
          (cons n nums)
          (cons (car nums) (insert n (cdr nums)))))) 
;(equal? (insert 1 '()) '(1))
;(equal? (insert 1 '(2 3)) '(1 2 3))
;(equal? (insert 4 '(1 2 3)) '(1 2 3 4))
;(equal? (insert 2 '(1 3)) '(1 2 3))

;insertion-sort : List -> List
(define (insertion-sort nums)
  (if (null? nums)
      '()
      (insert (car nums) (insertion-sort (cdr nums)))))
;(equal? (insertion-sort '()) '())
;(equal? (insertion-sort '(1)) '(1))
;(equal? (insertion-sort '(1 2 3 4 5)) '(1 2 3 4 5))
;(equal? (insertion-sort '(5 2 1 4 3)) '(1 2 3 4 5))
;(equal? (insertion-sort '(9 8 7 6 5 4 3 2 1 0)) '(0 1 2 3 4 5 6 7 8 9))

