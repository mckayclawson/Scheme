;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1B Question2

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

;odd-indexes : List -> List
;a helper function to return a sublist of all the odd indexed elements
(define (odd-indexes L)
  (if (null? L)
      '()
      (if (null? (cdr L))
          '()
          (cons (cadr L) (odd-indexes (cdr (cdr L)))))))
;(equal? (odd-indexes '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
;(equal? (odd-indexes '()) '())
;(equal? (odd-indexes '(1)) '())
;(equal? (odd-indexes '(1 2)) '(2))

;even-indexes : List -> List
;a helper function to returna a sublist of all the even indexed elements
(define (even-indexes L)
  (if (null? L)
      '()
      (if (null? (cdr L))
          (list (car L))
          (cons (car L) (even-indexes (cdr (cdr L)))))))
;(equal? (even-indexes '(1 2 3 4 5 6 7 8 9)) '(1 3 5 7 9))
;(equal? (even-indexes '()) '())
;(equal? (even-indexes '(1)) '(1))
;(equal? (even-indexes '(1 2)) '(1))


;merge-sort : List -> List
(define (merge-sort nums)
  (if (null? nums)
      '()
      (if (null? (cdr nums))
          nums
          (merge (merge-sort (even-indexes nums)) (merge-sort (odd-indexes nums))))))
;(equal? (merge-sort '(9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9))
;(equal? (merge-sort '(1 2 3 4 5 6 7 8 9)) '(1 2 3 4 5 6 7 8 9))
;(equal? (merge-sort '(1 3 2 5 4 7 6 9 8)) '(1 2 3 4 5 6 7 8 9))
;(equal? (merge-sort '()) '())
;(equal? (merge-sort '(1)) '(1))
          


  
  
  