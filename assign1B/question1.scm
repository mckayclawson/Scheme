;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1B Question1

;merge : List * List -> List 
(define (merge ordered1 ordered2)
  (if (null? ordered1) ordered2
      (if (null? ordered2) ordered1
          (if (< (car ordered1) (car ordered2))
          (cons (car ordered1) (merge (cdr ordered1) ordered2))
          (cons (car ordered2) (merge ordered1 (cdr ordered2)))))))

;(equal? (merge '(1 3 5 7 9) '(2 4 6 8 10)) '(1 2 3 4 5 6 7 8 9 10))
;(equal? (merge '() '()) '())
;(equal? (merge '() '(1 2 3 4)) '(1 2 3 4))
;(equal? (merge '(1 2 3 4) '()) '(1 2 3 4))
;(equal? (merge '(1 2 3 4 5) '(6 7 8 9 10)) '(1 2 3 4 5 6 7 8 9 10))
  