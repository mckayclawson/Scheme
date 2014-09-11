;Author: McKay Clawson
;Programming Language Concepts
;Professor Nunes-Harwitt
;Assignment 1A Question2

;one? : List -> Boolean
(define (one? L)
  (cond ((null? L) #f)
        ((equal? (cdr L) '()) #t)
        (else #f)))
      
;(equal? (one? '()) #f)
;(equal? (one? '(x)) #t)
;(equal? (one? '(x y)) #f)
