;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 2A Question1

;union : List * List -> List
(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (equal? (member (car set1) set2) #f)) (union (cdr set1) set2))
        (else (cons (car set1) (union (cdr set1) set2)))))
;(equal? (union '() '()) '())
;(equal? (union '(1) '()) '(1))
;(equal? (union '() '(1)) '(1))
;(equal? (union '(1 2 3 4) '(5 6 7 8 9)) '(1 2 3 4 5 6 7 8 9))
;(equal? (union '(1 2 3 4) '(5 2 6 3)) '(1 4 5 2 6 3))
;(equal? (union '(1 2 3 4) '(1 2 3 4)) '(1 2 3 4))
;(equal? (union '(1 2 3 4) '(3)) '(1 2 4 3))
